{-|

THE XOR GAME

This file implements the server for a game in which each player makes 2 choices:
which bit, zero or one, to XOR with every other player's choice of bit;
and which bit, zero or one, the player bets will be the outcome of this XOR.
The XOR takes place every twenty seconds. A player gains a point when
their bet matches the outcome. Points accumulate without limit or reward.

The game takes place in a chatroom. The first message the client sends to the
server is the client's nickname. After that, most messages are interpreted as
simple messages broadcast to everyone playing the game.
A message is instead interpreted as a command if it has the form "/$cmd $bit"
where $cmd is either "play" or "bet", and $bit is either "0" or "1".

I chose to implement this game because it is in some sense "minimal":
each player has exactly one bit of influence on the outcome. On the other hand,
in some sense every player holds the outcome of the game in her hands!

Previously, I tried to implement this game using a simple HTTP server
but I quickly realized that the request-response model is insufficient
to model a game which takes place in real time. So I used WebSockets.
Seeing that the standard example provided in the Haskell WebSockets library
documentation is a simple chat server, this implementation was easy and short.

I hope you enjoy reading it! Suggestions and pull requests welcome.

-}

{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TemplateHaskell #-}

import Control.Concurrent           (forkIO, threadDelay,
                                     MVar, newMVar, modifyMVar_, readMVar)
import Control.Exception            (fromException)
import Control.Lens                 (makeLenses, over, view, set)
import Control.Monad                (forever, forM, forM_)
import Control.Monad.IO.Class       (liftIO)
import Data.Aeson                   (Value(Number, String), ToJSON, toJSON,
                                     encode, object, (.=))
import Data.ByteString.Lazy.Char8   (unpack)
import Data.Monoid                  ((<>))
import Data.Function                (on)
import Data.Text                    (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS


-- data types
data Bit = Zero | One deriving (Show, Read, Eq, Ord)
instance Num Bit where
  x + y         = if x == y then Zero else One
  One * One     = One
  _   * _       = Zero
  abs           = id
  signum        = id
  fromInteger n = if even n then Zero else One

data Command = Play Bit | Bet Bit deriving (Show, Read, Eq, Ord)

data Client = Client {
    _nick     :: Text
  , _score    :: Int
  , _play     :: Bit
  , _bet      :: Bit
  , _channel  :: WS.Sink WS.Hybi00
  } deriving Eq
type ServerState = [Client]

$(makeLenses ''Client)

-- only encodes publically viewable client fields
instance ToJSON Client where
   toJSON client = object ["name"  .= String (view nick client),
                           "score" .= Number (fromIntegral $ view score client)]

-- string corresponds to the above JSON encoding (of just name and score)
instance Show Client where
  show = unpack . encode . toJSON

-- client helper functions
newClient :: Text -> WS.Sink WS.Hybi00 -> Client
newClient    name    chan               =
  Client {_nick = name, _score = 0, _play = 0, _bet = 0, _channel = chan}

clientExists :: Client -> ServerState -> Bool
clientExists = any . ((==) `on` view nick)

-- main loop will use clientExists to ensure that there are no duplicate names
addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient = filter . (/=)

incrementScore :: Client -> Client
incrementScore  = score `over` (+1)

updateClient :: Command -> Client -> Client
updateClient    (Play b) = set play b
updateClient    (Bet  b) = set bet b


-- chat room functions
broadcast :: Text -> ServerState -> IO ()
broadcast    msg     clients      = do
  T.putStrLn msg -- log to console
  forM_ clients $ \client ->
    WS.sendSink (view channel client) (WS.textData msg)

parseMessage :: Text -> Either Text (Maybe Command)
parseMessage    msg   = if T.head msg /= '/'
                        then Left msg -- simple chat message
                        else Right $ case msg of
                          "/play 0" -> Just $ Play Zero
                          "/play 1" -> Just $ Play One
                          "/bet 0"  -> Just $ Bet Zero
                          "/bet 1"  -> Just $ Bet One
                          _         -> Nothing -- not a command


-- game play
xor :: ServerState -> Bit
xor = sum . map (view bet)

scores :: ServerState -> Text
scores = T.pack . concatMap show


-- main logic
main :: IO ()
main  = do
  T.putStrLn "opened XOR game chat room..."
  state <- newMVar []
  _     <- forkIO $ xorAndUpdate state
  WS.runServer "127.0.0.1" 8000 $ game state

delay :: Int
delay = 2 * 10^7 -- twenty seconds

-- Every delay seconds, compare each connected client's bet to the xor of all
-- the clients' plays: if it is different, the client's state is unaltered;
--                     if it is the same, the client's score is incremented
xorAndUpdate :: MVar ServerState -> IO ()
xorAndUpdate    state             = forever $ do
  threadDelay delay
  modifyMVar_ state $ \clients -> do
      updatedClients <- forM clients $ \client ->
          if xor clients == view bet client
          then return $ incrementScore client
          else return client
      broadcast (scores updatedClients) updatedClients
      return updatedClients

-- TODO: refactor this into smaller functions
game :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
game    state               req         = do
  WS.acceptRequest req
  sink    <- WS.getSink
  WS.sendTextData ("choose a nickname" :: Text)
  name    <- WS.receiveData
  let client = newClient name sink
  clients <- liftIO $ readMVar state
  if clientExists client clients
    then do WS.sendTextData ("***that nick is taken, choose another***" :: Text)
            game state req -- BUG: this closes the connection and doesn't loop!
    else do liftIO $ modifyMVar_ state $ \s -> do
                let s' = addClient client s
                WS.sendSink sink $ WS.textData $ "***XOR GAME! Players: "
                  <> T.intercalate ", " (map (view nick) s) <> "***"
                broadcast (view nick client <> " joined") s
                return s'
            talk state client


talk :: MVar ServerState -> Client -> WS.WebSockets WS.Hybi00 ()
talk    state               player  =
  flip WS.catchWsError catchDisconnect $ forever $ do
      input <- WS.receiveData
      case parseMessage input of
        Left msg         -> liftIO $ readMVar state >>= 
                                     broadcast (view nick player <> ": " <> msg)
        Right Nothing    -> WS.sendTextData ("*couldn't parse command*" :: Text)
        Right (Just cmd) -> liftIO $ modifyMVar_ state $ \clients ->
          let newPlayer = updateClient cmd player
          in  return $ addClient newPlayer $ removeClient player clients

  where
    catchDisconnect e = case fromException e of
      Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
          let s' = removeClient player s
          broadcast (view nick player <> " disconnected") s'
          return s'
      _                        -> return ()

{-|

THE XOR GAME

This file implements the server for a game in which each player makes 2 choices:
which bit, zero or one, to XOR with every other player's choice of bit;
and which bit, zero or one, the player bets will be the outcome of this XOR.
The XOR takes place every ten seconds. A player gains a point when
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

{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

import Control.Concurrent     (forkIO, threadDelay,
                               MVar, newMVar, modifyMVar_, readMVar)
import Control.Exception      (fromException)
import Control.Monad          (forever, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid            ((<>))
import Data.Text              (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS


-- a hack so that we can print out a representation of our clients a bit later
instance Show (WS.Sink a) where
  show _ = "some version of the WebSockets protocol"


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

-- note that in Haskell, using record syntax to declare a data type brings into
-- scope accessor functions with the same names as the fields.
-- An example used below in my function `xor` is play :: Client -> Bit
data Client = Client {
    nick :: Text
  , score    :: Int
  , play     :: Bit
  , bet      :: Bit
  , channel  :: WS.Sink WS.Hybi00
  } deriving (Show, Eq)

type ServerState = [Client]


-- dealing with user input
parseMessage :: Text -> Either Text (Maybe Command)
parseMessage    msg   = if T.head msg /= '/'
                        then Left msg -- simple chat message
                        else Right $ case msg of
                          "/play 0" -> Just $ Play Zero
                          "/play 1" -> Just $ Play One
                          "/bet 0"  -> Just $ Bet Zero
                          "/bet 1"  -> Just $ Bet One
                          _         -> Nothing -- not a command

-- ugly! perhaps use lenses?
updateClient :: Command -> Client                                       -> ServerState
                -> IO ServerState
updateClient    (Play bit) client@Client {nick, score, play, bet, channel} clients = do
  return $ addClient Client {nick, score, play = bit, bet, channel} $ 
            removeClient client clients
updateClient    (Bet  bit) client@Client {nick, score, play, bet, channel} clients = do
  return $ addClient Client {nick, score, play, bet = bit, channel} $ 
            removeClient client clients


-- chat room functions

clientExists :: Client -> ServerState -> Bool
clientExists    client                 = any (== nick client) . map nick

-- main loop will check to ensure that there are no duplicate names
addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient = filter . (/=)

broadcast :: Text -> ServerState -> IO ()
broadcast    msg     clients      = do
  T.putStrLn msg
  forM_ clients $ \client -> WS.sendSink (channel client) (WS.textData $ msg <> "\n")


-- game play

xor :: ServerState -> Bit
xor = sum . map play -- get the bits out of the clients, then add them mod 2

scores :: ServerState -> Text
scores    clients      =
  let showPair :: (Text, Int) -> Text
      showPair    (str,  n)     = str <> ": " <> T.pack (show n) <> " points"
  in T.concat $ map showPair $ zip (map nick clients) (map score clients)


main :: IO ()
main  = do
  T.putStrLn "opened XOR game chat room..."
  state <- newMVar []
  _     <- forkIO $ xorAndUpdate state
  WS.runServer "127.0.0.1" 8000 $ game state

delay :: Int
delay  = 2 * 10^7 -- 20 seconds

-- Compare each connected client's bet to the xor of all the clients' plays:
-- if it is different, the client's state is unalterered;
-- if it is the same, the client's score is incremented
xorAndUpdate :: MVar ServerState -> IO ()
xorAndUpdate    state             = forever $ do
  threadDelay delay
  modifyMVar_ state $ \clients -> do
      updatedClients <- forM clients $ \client ->
        return $ if bet client /= xor clients
                 then client
                 else Client { -- even uglier! look into lenses for sure
                   nick    = nick client
                   , score   = score client + 1
                   , play    = play client
                   , bet     = bet client
                   , channel = channel client
                   }
      broadcast ("Scores:\n" <> scores updatedClients) updatedClients
      return updatedClients

game :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
game    state               req         = do
  WS.acceptRequest req
  WS.sendTextData ("choose a nickname" :: Text)
  sink <- WS.getSink
  name <- WS.receiveData
  clients <- liftIO $ readMVar state
  let client = Client {nick = name, score = 0, play = 0, bet = 0, channel = sink}
  if clientExists client clients
    then do WS.sendTextData ("That nick is taken, choose another" :: Text)
            game state req
    else do liftIO $ modifyMVar_ state $ \s -> do
                let s' = addClient client s
                WS.sendSink sink $ WS.textData $
                  "XOR GAME! Players: " <> T.intercalate ", " (map nick s)
                broadcast (nick client <> " joined") s'
                return s'
            talk state client

talk :: MVar ServerState -> Client -> WS.WebSockets WS.Hybi00 ()
talk    state               client  =
  flip WS.catchWsError catchDisconnect $ forever $ do
      input <- WS.receiveData
      case parseMessage input of
        Left msg         -> liftIO $ do
          clients <- readMVar state
          broadcast (nick client <> ": " <> msg) clients
            
        Right Nothing    -> WS.sendTextData ("*couldn't parse command*" :: Text)

        Right (Just cmd) -> liftIO $ modifyMVar_ state $ \clients ->
          updateClient cmd client clients
  where
    catchDisconnect e = case fromException e of
      Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
          let s' = removeClient client s
          broadcast (nick client <> " disconnected") s'
          return s'
      _                        -> return ()


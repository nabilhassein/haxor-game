{-|

THE XOR GAME

This file implements the server for a game in which each player makes 2 choices:
which bit, zero or one, to XOR with every other player's choice of bit;
and which bit, zero or one, the player bets will be the outcome of this XOR.
The XOR takes place every twenty seconds. A player gains a point when
their bet matches the outcome. Points accumulate without limit or reward.

I chose to implement this game because it is in some sense "minimal":
each player has exactly one bit of influence on the outcome. On the other hand,
in some sense every player holds the outcome of the game in her hands!

Previously, I tried to implement this game using a simple HTTP server
but I quickly realized that the request-response model is insufficient
to model a game which takes place in real time. So I used WebSockets.
Communication with the client takes place with JSON messages.

I hope you enjoy reading it! Suggestions and pull requests welcome.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Prelude                 hiding (putStrLn)
import Control.Applicative            ((<$>), (<*>))
import Control.Concurrent             (forkIO, threadDelay,
                                       MVar, newMVar, modifyMVar_, readMVar)
import Control.Exception              (fromException)
import Control.Lens                   (makeLenses, over, view, set)
import Control.Monad                  (forever, forM, forM_)
import Control.Monad.IO.Class         (liftIO)
import Data.Aeson                     (Value(Number, Object), ToJSON, FromJSON,
                                       (.=), (.:?), object, toJSON, parseJSON,
                                       encode, decode)
import Data.Aeson.Encode              (fromValue)
import Data.Attoparsec.Number         (Number(I))
import Data.Text                      (Text, append, pack)
import Data.Text.IO                   (putStrLn)
import Data.Text.Lazy                 (toStrict)
import Data.Text.Lazy.Builder         (toLazyText)
import qualified Network.WebSockets as WS



-- data types and their instances

data Bit = Zero | One deriving (Show, Read, Eq, Ord)

instance Num Bit where
  x + y         = if x == y then Zero else One
  One * One     = One
  _   * _       = Zero
  abs           = id
  signum        = id
  fromInteger n = if even n then Zero else One

instance ToJSON Bit where
  toJSON Zero = Number 0
  toJSON One  = Number 1

instance FromJSON Bit where
  parseJSON (Number (I k)) = return $ fromInteger k
  parseJSON _              = fail "not a bit"


data Update = Update {
    updatePlay :: Maybe Bit
  , updateBet  :: Maybe Bit
  } deriving Show

-- ugly!
instance ToJSON Update where
  toJSON update = case (updatePlay update, updateBet update) of
    (Just playBit, Just betBit) -> object ["play" .= playBit, "bet" .= betBit]
    (Just playBit, Nothing    ) -> object ["play" .= playBit]
    (Nothing     , Just betBit) -> object ["bet"  .= betBit ]
    (Nothing     , Nothing    ) -> object []


instance FromJSON Update where
  -- All fields are optional, so we accept ANY object. Reject other JSON types.
  parseJSON (Object v) = Update <$> v .:? "play" <*> v .:? "bet"
  parseJSON _          = fail "invalid update"


type ServerState = [Client]

data Client = Client {
    _nick    :: Text
  , _score   :: Int
  , _play    :: Bit
  , _bet     :: Bit
  , _channel :: WS.Sink WS.Hybi10
  }

$(makeLenses ''Client)

-- { _nick: _score }
instance ToJSON Client where
   toJSON client = object [(view nick client) .=
                           Number (fromIntegral $ view score client)]

instance Show Client where
  show = show . toJSON



-- Client helper functions

newClient :: Text -> WS.Sink WS.Hybi10 -> Client
newClient    name    chan               = Client {
    _nick    = name
  , _score   = 0
  , _play    = 0
  , _bet     = 0
  , _channel = chan
  }

nameExists :: Text -> ServerState -> Bool
nameExists    name  = any (== name) . map (view nick)

-- main loop uses nameExists before this function can be reached
addClient :: Client -> ServerState -> ServerState
addClient  = (:)

-- remove clients by name, ignoring other field
removeClient :: Client -> ServerState -> ServerState
removeClient    client  = filter (\c -> view nick c /= view nick client)

incrementScore :: Client -> Client
incrementScore  = score `over` (+1)

-- ugly!
updateClient :: Update -> Client -> Client
updateClient    update    client = case (updatePlay update, updateBet update) of
  (Just playBit, Just betBit) -> set play playBit $ set bet betBit client
  (Just playBit, Nothing    ) -> set play playBit client
  (Nothing     , Just betBit) -> set bet  betBit client
  (Nothing     , Nothing    ) -> client



-- game play
xor :: ServerState -> Bit
xor  = sum . map (view bet)



-- client-server communication

-- Any Object is parsed into a Right Update; all keys except "play" and "bet"
-- are ignored. The value should be a 0 or 1 in either case.
-- Anything else (String, Number, etc.) is transmitted as chat (Left Text)
-- TODO: better error handling
parseMessage :: Text -> Either Text Update
parseMessage    msg   = case decode (encode $ show msg) of -- TODO: double check
  Nothing -> Left msg
  Just u  -> Right u

-- helper functions below ensure msg has correct JSON encoding
broadcast :: Text -> ServerState -> IO ()
broadcast    msg     clients      = do
  putStrLn msg -- log to console
  forM_ clients $ \client ->
    WS.sendSink (view channel client) (WS.textData msg)

-- for these next functions, cf. client.js's onMessage

-- TODO: double check this
encodeObject :: ToJSON a => Text     -> a -> Text
encodeObject                typename    obj =
  toStrict . toLazyText . fromValue $ object ["type" .= typename, "data" .= obj]

warning :: Text -> Text
warning    msg   = encodeObject "warning" $ object ["warning" .= msg]

initialize :: ServerState -> Text
initialize    clients      =
  encodeObject "initialize" $ object ["bet"  .= Zero, "scoreboard" .= clients,
                                      "play" .= Zero, "result" .= xor clients]

confirmUpdate :: Update -> Text
confirmUpdate  = encodeObject "update"

scoreboard :: ServerState -> Text
scoreboard    clients      = encodeObject "scoreboard" $
                             object ["scoreboard" .= clients,
                                     "result"     .= xor clients]

joined :: Client -> Text
joined    client  = encodeObject "joined" $ object ["name" .= view nick client]

left :: Client -> Text
left    client  = encodeObject "left" $ object ["name" .= view nick client]

chat :: Client -> Text -> Text
chat    client    msg   = encodeObject "chat" $
  object ["message" .= (view nick client `append` ": " `append` msg)]



-- main logic
main :: IO ()
main  = do
  putStrLn "opened XOR game chat room..."
  state <- newMVar []
  _     <- forkIO $ xorAndUpdate state
  WS.runServer "127.0.0.1" 8000 $ game state

-- Every delay microseconds, compare each connected client's bet to the xor of
-- all the clients' plays: if it is different, the client's state is unaltered;
--                         if it is the same, the client's score is incremented
xorAndUpdate :: MVar ServerState -> IO ()
xorAndUpdate    state             = forever $ do
    threadDelay delay
    modifyMVar_ state $ \clients -> do
        updatedClients <- forM clients $ \client ->
            return $ if xor clients == view bet client
                     then incrementScore client
                     else client
        broadcast (scoreboard updatedClients) updatedClients
        return updatedClients
    where
      delay = 2 * 10^7 -- twenty seconds

-- possibility of subtle races, resulting in two people with same name?
getName :: MVar ServerState -> WS.WebSockets WS.Hybi10 Text
getName    state             = do
  name    <- WS.receiveData
  clients <- liftIO $ readMVar state
  if nameExists name clients
    then do WS.sendTextData $ warning "that nick is taken, choose another"
            getName state
    else return name

game :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
game    state               req         = do
  WS.acceptRequest req
  sink <- WS.getSink
  name <- getName state
  let client = newClient name sink
  liftIO $ modifyMVar_ state $ \clients -> do
      WS.sendSink sink $ WS.textData $ initialize clients
      broadcast (joined client) clients
      return $ addClient client clients
  talk state client

talk :: MVar ServerState -> Client -> WS.WebSockets WS.Hybi10 ()
talk    state               player  =
  flip WS.catchWsError catchDisconnect $ forever $ do
      input <- WS.receiveData
      case parseMessage input of
        Left  msg    -> liftIO $ readMVar state >>= broadcast (chat player msg)
        Right update -> do
            liftIO $ modifyMVar_ state $ \clients ->
                let newPlayer = updateClient update player
                in  return $ addClient newPlayer $ removeClient player clients
            WS.sendTextData $ confirmUpdate update
  where
    catchDisconnect e = case fromException e of
      Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
          let s' = removeClient player s
          broadcast (left player) s'
          return s'
      _  -> liftIO . putStrLn $ "unexpected error: " `append`
            pack (show player)  `append` " encountered "  `append` pack (show e)

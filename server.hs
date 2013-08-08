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
                               MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
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

instance Show (MVar a) where
  show _ = "some mutable variable"

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
    nick     :: Text
  , score    :: MVar Int
  , play     :: MVar Bit
  , bet      :: MVar Bit
  , channel  :: WS.Sink WS.Hybi00
  } deriving (Show, Eq)

type ServerState = [Client]


newClient :: Text -> WS.Sink WS.Hybi00 -> IO Client
newClient    name    chan              = do
  points <- newMVar 0
  input  <- newMVar 0
  gamble <- newMVar 0
  return Client{nick=name, score=points, play=input, bet=gamble, channel=chan}

clientExists :: Client -> ServerState -> Bool
clientExists    client                 = any (== nick client) . map nick

addClient :: Client -> ServerState -> ServerState
addClient = (:)

-- main loop will check to ensure that there are no duplicate names
removeClient :: Client -> ServerState -> ServerState
removeClient = filter . (/=)

updateClient :: Command -> Client -> IO Client
updateClient    cmd        client  =
  let (accessor, bit) = case cmd of
        (Play b) -> (play, b)
        (Bet  b) -> (bet,  b)
  in modifyMVar (accessor client) $ \_ -> return (bit, client)


-- chat room functions
broadcast :: Text -> ServerState -> IO ()
broadcast    msg     clients      = do
  T.putStrLn msg
  forM_ clients $ \client ->
    WS.sendSink (channel client) (WS.textData $ msg <> "\n")


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
xor :: ServerState -> IO Bit
xor    clients = do
  let inputs = map play clients
  bits <- sequence $ map readMVar inputs
  return $ sum bits

incrementScore :: Client -> IO Client
incrementScore    client = let points = score client in do
  modifyMVar points $ \n -> return (n + 1, client)

scores :: ServerState -> Bit  -> IO Text
scores    clients        result =
  let showPair :: (Text, MVar Int) -> IO Text
      showPair    (name, score   )  = do
          n <- readMVar score
          return $ T.concat ["- ", name, ": ", T.pack (show n), " points\n"]
      getPlayerScores :: IO Text
      getPlayerScores  = fmap T.concat . sequence $ map showPair $
                         zip (map nick clients) (map score clients)
  in do
    playerScores <- getPlayerScores
    return $ T.concat ["END OF ROUND! The result was ", T.pack (show result),
               ".\nUpdated Scores:\n", playerScores]



main :: IO ()
main  = do
  T.putStrLn "opened XOR game chat room..."
  state <- newMVar []
  _     <- forkIO $ xorAndUpdate state
  WS.runServer "127.0.0.1" 8000 $ game state

delay :: Int
delay  = 2 * 10^7 -- 20 seconds

-- Compare each connected client's bet to the xor of all the clients' plays:
-- if it is different, the client's state is unaltered;
-- if it is the same, the client's score is incremented
xorAndUpdate :: MVar ServerState -> IO ()
xorAndUpdate    state             = forever $ do
  threadDelay delay
  modifyMVar_ state $ \clients -> do
    result         <- xor clients
    updatedClients <- forM clients $ \client -> do
        clientBet <- readMVar $ bet client
        if clientBet /= result
          then return client
          else incrementScore client >>= return
    scoreBoard <- scores updatedClients result
    broadcast scoreBoard updatedClients
    return updatedClients

game :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
game    state               req         = do
  WS.acceptRequest req
  sink    <- WS.getSink
  WS.sendTextData ("choose a nickname" :: Text)
  name    <- WS.receiveData
  client  <- liftIO $ newClient name sink
  clients <- liftIO $ readMVar state
  if clientExists client clients
    then do WS.sendTextData ("***that nick is taken, choose another***" :: Text)
            game state req -- BUG: this closes the connection and doesn't loop!
    else do liftIO $ modifyMVar_ state $ \s -> do
                let s' = addClient client s
                WS.sendSink sink $ WS.textData $ "***XOR GAME! Players: "
                  <> T.intercalate ", " (map nick s) <> "***"
                broadcast (nick client <> " joined") s'
                return s'
            talk state client


talk :: MVar ServerState -> Client -> WS.WebSockets WS.Hybi00 ()
talk    state               player  =
  flip WS.catchWsError catchDisconnect $ forever $ do
      input <- WS.receiveData
      case parseMessage input of
        Left msg         -> liftIO $ do
          clients <- readMVar state
          broadcast (nick player <> ": " <> msg) clients
            
        Right Nothing    -> WS.sendTextData ("*couldn't parse command*" :: Text)

        Right (Just cmd) -> liftIO $ modifyMVar_ state $ \clients ->
          forM clients $ \client -> updateClient cmd client
  where
    catchDisconnect e = case fromException e of
      Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
          let s' = removeClient player s
          broadcast (nick player <> " disconnected") s'
          return s'
      _                        -> return ()

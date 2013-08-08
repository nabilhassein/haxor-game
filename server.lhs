{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent     (MVar, newMVar, modifyMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text)
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS


initialMessage :: Text
initialMessage  = "You are playing the XOR game, in which n bits are XORed.\n\
\You get to choose one of those bits. You also get to bet on 0 or 1.\n\
\You win if the XOR of all n bits is the same as your bet. You lose if not.\n\
\The counter is reset every 60 seconds."

winMessage :: Text
winMessage  = "You won! Play again."

loseMessage :: Text
loseMessage  = "You lost! Play again."

badInputMessage :: Text
badInputMessage  = "Couldn't understand that input. Try playing again."

data Bit     = Zero | One deriving (Show, Read, Eq, Ord)
data Command = Play Bit | Bet Bit deriving (Show, Read, Eq, Ord)

-- Players modify their own state with commands that begin with a '/'.
-- Those commands are "/play" and "/bet".
-- "/$foo" where $foo is any string other than "play" or "bet" raises
-- Right Nothing, the case of a misunderstood command.
-- "/play" or "/bet", on the 
-- Left Text is the ordinary case: the user sent simple text over the wire
parse :: Text ->        Either Text (Maybe (Command Bit))
parse    msg@(h, t)   = if h == '/'
                        then Command T.


main :: IO ()
main = do
  T.putStrLn "waiting for players..."
  counter <- newMVar 0
  WS.runServer "127.0.0.1" 8000 $ app counter

app :: MVar Int -> WS.Request -> WS.WebSockets WS.Hybi00 ()
app    counter     req         = do
  WS.acceptRequest req
  liftIO $ T.putStrLn "player joined"
  WS.sendTextData initialMessage
  loop
  where
    loop = flip WS.catchWsError catchDisconnect $ do
      msg <- WS.receiveData
      case parse msg of
        Nothing           -> WS.sendTextData badInputMessage >> loop
        Just (play, bet) -> do
          win <- liftIO $ modifyMVar counter $ \n ->
              let n' = n + input
              in  return (n', n' `mod` 2 == bet)
          WS.sendTextData $ if win then winMessage else loseMessage
          loop
      
    catchDisconnect _ = liftIO $ T.putStrLn "player left"

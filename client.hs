{-# LANGUAGE OverloadedStrings #-}

import Control.Monad       (forever)
import Control.Concurrent  (forkIO)
import Control.Monad.Trans (liftIO)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS


main :: IO ()
main = WS.connect "127.0.0.1" 8000 "/" app

app :: WS.WebSockets WS.Hybi00 ()
app = do
    WS.getSink >>= liftIO . forkIO . sendMessages
    forever $ WS.receiveData >>= liftIO . T.putStrLn

sendMessages :: (WS.TextProtocol p) => WS.Sink p -> IO ()
sendMessages                           sink       = forever $
  T.getLine >>= WS.sendSink sink . WS.textData

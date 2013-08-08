{-# LANGUAGE OverloadedStrings #-}

import Control.Monad       (forever)
import Control.Concurrent  (forkIO)
import Control.Monad.Trans (liftIO)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS


app :: WS.WebSockets WS.Hybi10 ()
app = do
    -- Fork off a separate thread that reads from stdin and writes to the sink.
    sink <- WS.getSink
    _ <- liftIO $ forkIO $ readInput sink
    forever $ WS.receiveData >>= liftIO . T.putStrLn

readInput :: (WS.TextProtocol p) => WS.Sink p -> IO ()
readInput sink = forever $ T.getLine >>= WS.sendSink sink . WS.textData

main :: IO ()
main = WS.connect "127.0.0.1" 8000 "/" app

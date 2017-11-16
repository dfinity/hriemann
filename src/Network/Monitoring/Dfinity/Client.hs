module Network.Monitoring.Dfinity.Client (connect, send, Client) where

import           Network.Monitoring.Riemann.BatchClient (batchClient)
import           Network.Monitoring.Riemann.Client      (sendEvent)
import           Network.Monitoring.Riemann.Event       (Event)

import           Control.Concurrent                     (forkIO, threadDelay)
import           Control.Concurrent.BoundedChan         (readChan, tryWriteChan, BoundedChan, newBoundedChan)
import           Control.Exception                      (handle, SomeException)
import Control.Monad (forever, void)

backoffDelay :: Int
backoffDelay = 5000000 -- 5s

bufferSize :: Int
bufferSize = 1000

batchSize :: Int
batchSize = 100

newtype Client = Client (BoundedChan Event)

send :: Client -> Event -> IO ()
send (Client ch) ev = void $ tryWriteChan ch ev

-- Establish connection with a monitoring server and returns a client.
connect :: String -> IO Client
connect addr = do
  -- channel of events
  ch <- newBoundedChan bufferSize

  -- Launch a thread that establishes connections with the server
  -- and sends events.
  void $ forkIO $ clientLoop addr ch

  return $ Client ch

-- The clientLoop establishes connection with the monitoring server,
-- reads events off the given channel, and send events  to the server.
-- If any of these steps fails, it restarts with some backoff.
clientLoop :: String -> BoundedChan Event -> IO ()
clientLoop addr ch = forever $ handle onEx $ do
  cli <- batchClient addr 5555 bufferSize batchSize silentDrop
  forever $ do
    ev <- readChan ch
    sendEvent cli ev

  where
    onEx e = let _ = (e :: SomeException) in do
      -- TODO: log the exception
      -- TODO: exponential backoff?
      -- We want to backoff a little bit so that, for instance, if the
      -- monitoring server is down, we don't want to be trying to connect
      -- with it in a busy loop.
      putStrLn $ "Exception: " ++ show e
      threadDelay backoffDelay

    silentDrop _ = return ()

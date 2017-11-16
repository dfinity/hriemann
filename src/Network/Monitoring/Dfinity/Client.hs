module Network.Monitoring.Dfinity.Client (connect, send, Client) where

import Network.Monitoring.Riemann.BatchClient (BatchClient, batchClient)
import Network.Monitoring.Riemann.Client (sendEvent)
import Network.Monitoring.Riemann.Event (Event)

newtype Client = Client BatchClient

-- A convenience function for connecting with a full address like
-- "localhost:3456"
connect :: String -> IO Client
connect addr = do
  cli <- batchClient addr 5555 1000 100 silentDrop
  return $ Client cli
  where
    silentDrop _ = return ()

send :: Client -> Event -> IO ()
send (Client cli) = sendEvent cli
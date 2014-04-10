module Main where

import Control.Concurrent
import Control.Exception  (bracket)
import Control.Monad      (forever, liftM2, unless)

import Data.Function      (on)
import Data.List          (deleteBy)
import Data.UUID          (UUID)

import System.UUID.V4     (uuid)

import qualified Network.WebSockets as WS
import qualified Data.Text          as T
import qualified Data.Text.IO       as T


main :: IO ()
main = do
  st <- initState
  _  <- forkIO $ WS.runServer "0.0.0.0" 9160 $ serve st
  forever $ T.getLine >>= liftM2 unless T.null (announce st)

serve :: ServerState -> WS.PendingConnection -> IO ()
serve st pending = do
  conn <- WS.acceptRequest pending
  bracket
    (registerClient st conn)
    (unregisterClient st)
    (forever . talk)

talk :: Client -> IO ()
talk (Client _ conn chan) = readChan chan >>= WS.sendTextData conn

announce :: ServerState -> T.Text -> IO ()
announce st msg =
  readMVar (clients st) >>= mapM_ (flip writeChan msg . clientChan)

-- Server state

data Client = Client { clientId   :: UUID
                     , clientConn :: WS.Connection
                     , clientChan :: Chan T.Text
                     }

data ServerState = ServerState { clients :: MVar [Client] }

initState :: IO ServerState
initState = fmap ServerState $ newMVar []

registerClient :: ServerState -> WS.Connection -> IO Client
registerClient st conn = do
  client <- initClient conn
  modifyMVar_ (clients st) (return . (client :))
  putStrLn $ "Client registered: " ++ show (clientId client)
  return client

initClient :: WS.Connection -> IO Client
initClient conn = do
  cid  <- uuid
  chan <- newChan
  return $ Client cid conn chan

unregisterClient :: ServerState -> Client -> IO ()
unregisterClient st client = do
  modifyMVar_ (clients st) (return . deleteBy ((==) `on` clientId) client)
  putStrLn $ "Client unregistered: " ++ show (clientId client)

-- WS utils

receiveTextData :: WS.Connection -> IO T.Text
receiveTextData = WS.receiveData

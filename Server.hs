{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Concurrent
import Control.Monad              (forever, liftM2, unless)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)

import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO, bracket)

import Data.Function (on)
import Data.List     (deleteBy)
import Data.UUID     (UUID)

import System.UUID.V4 (uuid)

import qualified Network.WebSockets as WS
import qualified Data.Text          as T
import qualified Data.Text.IO       as T


main :: IO ()
main = do
  st <- initState
  _  <- forkIO $ WS.runServer "0.0.0.0" 9160 (runSt st serve)
  forever $ T.getLine >>= liftM2 unless T.null (runSt st announce)
    where
      runSt st f = flip runReaderT st . runServ . f

serve :: WS.PendingConnection -> Serv ()
serve pending = do
  conn <- liftIO $ WS.acceptRequest pending
  bracket
    (registerClient conn)
    unregisterClient
    (forever . talk)

talk :: Client -> Serv ()
talk (Client _ conn chan) =
  liftIO $ readChan chan >>= WS.sendTextData conn

announce :: T.Text -> Serv ()
announce msg = do
  clients <- askClients
  liftIO $ readMVar clients >>= mapM_ (flip writeChan msg . clientChan)


-- Server context monad

newtype Serv a = Serv { runServ :: ReaderT ServerState IO a }
                 deriving (Functor, Monad, MonadIO, MonadCatchIO)

-- Read-only server state

data Client = Client { clientId   :: UUID
                     , clientConn :: WS.Connection
                     , clientChan :: Chan T.Text
                     }

data ServerState = ServerState { clients :: MVar [Client] }

initState :: IO ServerState
initState = fmap ServerState $ newMVar []

registerClient :: WS.Connection -> Serv Client
registerClient conn = do
  client  <- initClient conn
  modifyClients (client :)
  consoleLog $ "Client registered: " ++ show (clientId client)
  return client

initClient :: WS.Connection -> Serv Client
initClient conn = do
  cid  <- liftIO uuid
  chan <- liftIO newChan
  return $ Client cid conn chan

unregisterClient :: Client -> Serv ()
unregisterClient client = do
  modifyClients $ deleteBy ((==) `on` clientId) client
  consoleLog $ "Client unregistered: " ++ show (clientId client)

modifyClients :: ([Client] -> [Client]) -> Serv ()
modifyClients f = askClients >>= liftIO . flip modifyMVar_ (return . f)

askClients :: Serv (MVar [Client])
askClients = Serv $ asks clients

consoleLog :: String -> Serv ()
consoleLog s = liftIO $ putStrLn s

-- WS utils

receiveTextData :: WS.Connection -> IO T.Text
receiveTextData = WS.receiveData

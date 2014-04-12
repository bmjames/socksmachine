{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Prelude hiding (mapM_)

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, swapMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

import Control.Monad              (forever, liftM2, unless, void)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)

import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO, bracket)

import Data.Foldable (mapM_)
import Data.Function (on)
import Data.List     (deleteBy)
import Data.UUID     (UUID)

import System.UUID.V4 (uuid)

import qualified Network.WebSockets as WS
import qualified Data.Text          as T
import qualified Data.Text.IO       as T


-- | Start a server, read lines from stdin and broadcast to all clients.
main :: IO ()
main = do
  st <- initState
  _  <- forkIO $ WS.runServer "0.0.0.0" 9160 (runSt st serve)
  forever $ T.getLine >>= liftM2 unless T.null (runSt st announce)

  where
    runSt st f = flip runReaderT st . runServ . f

-- | Set the MOTD, and notify any currently connected clients
announce :: T.Text -> Serv ()
announce msg = do
  setMotd msg
  clients <- askClients
  liftIO $ readMVar clients >>= mapM_ (flip writeChan msg . clientChan)


-- | Add client to server state, ensuring it is removed on error/disconnect.
-- Once registered, send messages from the client's channel indefinitely.
serve :: WS.PendingConnection -> Serv ()
serve pending = do
  conn <- liftIO $ WS.acceptRequest pending
  bracket
    (registerClient conn)
    unregisterClient
    (liftM2 (>>) copyMotd (liftIO . forever . talk))

  where
    talk (Client _ conn ch) = readChan ch >>= WS.sendTextData conn
    copyMotd c = readMotd >>= liftIO . mapM_ (writeChan $ clientChan c)

-- | Server context monad
newtype Serv a = Serv { runServ :: ReaderT ServerState IO a }
                 deriving (Functor, Monad, MonadIO, MonadCatchIO)

-- | Read-only server state
data ServerState = ServerState { serverClients :: MVar [Client]
                               , serverMotd    :: MVar (Maybe T.Text)
                               }

data Client = Client { clientId   :: UUID
                     , clientConn :: WS.Connection
                     , clientChan :: Chan T.Text
                     }

initState :: IO ServerState
initState = do
  clients <- newMVar []
  motd    <- newMVar Nothing
  return $ ServerState clients motd

registerClient :: WS.Connection -> Serv Client
registerClient conn = do
  client  <- liftIO $ initClient conn
  modifyClients (client :)
  consoleLog $ "Client registered: " ++ show (clientId client)
  return client

  where
    initClient conn = liftM2 (flip Client conn) uuid newChan

unregisterClient :: Client -> Serv ()
unregisterClient client = do
  modifyClients $ deleteBy ((==) `on` clientId) client
  consoleLog $ "Client unregistered: " ++ show (clientId client)

modifyClients :: ([Client] -> [Client]) -> Serv ()
modifyClients f = do
  clients <- askClients
  liftIO $ modifyMVar_ clients (return . f)

askClients :: Serv (MVar [Client])
askClients = Serv $ asks serverClients

readMotd :: Serv (Maybe T.Text)
readMotd = do
  motd <- Serv $ asks serverMotd
  liftIO $ readMVar motd

setMotd :: T.Text -> Serv ()
setMotd msg = do
  motd <- Serv $ asks serverMotd
  void . liftIO $ swapMVar motd (Just msg)
  consoleLog $ "Set MOTD: " ++ show msg

consoleLog :: String -> Serv ()
consoleLog s = liftIO $ putStrLn s

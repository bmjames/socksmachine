{-# LANGUAGE PackageImports #-}

module Main where

import Prelude hiding (mapM_)

import Command
import Monad

import Control.Concurrent

import Control.Monad              (forever, liftM2, void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Reader (asks)

import "MonadCatchIO-transformers" Control.Monad.CatchIO (bracket)

import Data.Foldable (mapM_)
import Data.Function (on)
import Data.List     (deleteBy, find)

import qualified Network.WebSockets as WS
import qualified Data.Text          as T


-- | Start a server, read lines from stdin and broadcast to all clients.
main :: IO ()
main = do
  st <- initState
  _  <- forkIO $ WS.runServer "0.0.0.0" 9160 (runServ' st . serveConn)
  runServ' st serverConsoleMain

-- | Read and parse input from the server console
serverConsoleMain :: Serv ()
serverConsoleMain = forever $ do
  line <- liftIO getLine
  case parseCommand line of
    Left  err -> consoleLog $ show err
    Right cmd -> act cmd

  where
    act (SetMotd msg) = announce msg
    act ClearMotd     = clearMotd
    act (Msg cId msg) = sendToClient cId msg

-- | Set the MOTD, and notify any currently connected clients
announce :: T.Text -> Serv ()
announce msg = do
  setMotd msg
  clients <- askClients
  liftIO $ readMVar clients >>= mapM_ (flip writeChan msg . clientChan)

-- | Send a message to a single client
sendToClient :: Integer -> T.Text -> Serv ()
sendToClient cId msg = do
  clients <- askClients >>= liftIO . readMVar
  let maybeClient = find ((== cId) . clientId) clients
  mapM_ (liftIO . flip writeChan msg . clientChan) maybeClient

-- | Add client to server state, ensuring it is removed on error/disconnect.
-- Once registered, send messages from the client's channel indefinitely.
serveConn :: WS.PendingConnection -> Serv ()
serveConn pending = do
  conn <- liftIO $ WS.acceptRequest pending
  bracket
    (registerClient conn)
    unregisterClient
    (liftM2 (>>) copyMotd (liftIO . forever . talk))

  where
    talk (Client _ conn ch) = readChan ch >>= WS.sendTextData conn
    copyMotd c = readMotd >>= liftIO . mapM_ (writeChan $ clientChan c)

registerClient :: WS.Connection -> Serv Client
registerClient conn = do
  client  <- initClient
  modifyClients (client :)
  consoleLog $ "Client registered: " ++ show (clientId client)
  return client

  where
    initClient = do
      nextId <- Serv $ asks serverNextId
      cId    <- liftIO $ takeMVar nextId
      liftIO $ putMVar nextId (cId + 1)
      chan   <- liftIO newChan
      return $ Client cId conn chan

unregisterClient :: Client -> Serv ()
unregisterClient client = do
  modifyClients $ deleteBy ((==) `on` clientId) client
  consoleLog $ "Client unregistered: " ++ show (clientId client)

readMotd :: Serv (Maybe T.Text)
readMotd = askMotd >>= liftIO . readMVar

setMotd :: T.Text -> Serv ()
setMotd msg = do
  motd <- askMotd
  void . liftIO $ swapMVar motd (Just msg)
  consoleLog $ "Set MOTD: " ++ show msg

clearMotd :: Serv ()
clearMotd = do
  askMotd >>= liftIO . void . flip swapMVar Nothing
  consoleLog "Cleared MOTD"

consoleLog :: String -> Serv ()
consoleLog s = liftIO $ putStrLn s

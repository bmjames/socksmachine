{-# LANGUAGE PackageImports #-}

module SocksMachine.Server where

import Prelude hiding (mapM_)

import SocksMachine.Command
import SocksMachine.Monad

import Control.Concurrent
import Control.Concurrent.Async   (concurrently)
import Control.Monad              (forever, unless, void)
import Control.Monad.IO.Class     (liftIO)

import "MonadCatchIO-transformers" Control.Monad.CatchIO (bracket)

import Data.Foldable (mapM_)
import System.Exit   (exitSuccess)

import qualified Network.WebSockets as WS
import qualified Data.Text          as T


serverMain :: String -> Int -> Serv ()
serverMain addr port = liftBaseOpDiscard (WS.runServer addr port) serveConn

-- | Read and parse input from the server console
serverConsoleMain :: Serv ()
serverConsoleMain = forever $ do
  line <- liftIO getLine
  unless (null line) $
    either (consoleLog . show) runCommand $ parseCommand line

runCommand :: Command -> Serv ()
runCommand cmd = case cmd of
  SetMotd msg  -> setMotd msg >> announce msg
  ClearMotd    -> clearMotd
  Announce msg -> announce msg
  Msg cId msg  -> findClient cId >>= liftIO . mapM_ (`message` msg)
  Shutdown     -> shutdown

-- | Send a message to all currently connected clients
announce :: T.Text -> Serv ()
announce msg = do
  clients <- askClients
  liftIO $ readMVar clients >>= mapM_ (flip writeChan msg . clientChan)

-- | Send a message to an individual client
message :: Client -> T.Text -> IO ()
message client = writeChan (clientChan client)

-- | Add client to server state, ensuring it is removed on error/disconnect.
-- Once registered, send messages from the client's channel indefinitely.
serveConn :: WS.PendingConnection -> Serv ()
serveConn pending = do
  conn <- liftIO $ WS.acceptRequest pending
  bracket (initClient conn) unregisterClient $ \client -> do
    registerClient client
    -- send MOTD if it is set
    sendMotd client
    liftIO . void $ concurrently
      (forever $ talk client)
      -- discard (non-control) messages sent by client
      (forever $ WS.receiveDataMessage conn)

  where
    sendMotd c = readMotd >>= liftIO . mapM_ (writeChan $ clientChan c)
    talk (Client _ conn ch) = readChan ch >>= WS.sendTextData conn

shutdown :: Serv ()
shutdown = do
  clients <- askClients
  liftIO $ do
    clients' <- takeMVar clients
    mapM_ (flip WS.sendClose msg . clientConn) clients'
    exitSuccess

  where msg = T.pack "The server is shutting down."

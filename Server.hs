{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Prelude hiding (mapM_)

import Control.Applicative
import Control.Concurrent

import Control.Monad              (forever, liftM2, liftM3, unless, void)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)

import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO, bracket)

import Data.Foldable (mapM_)
import Data.Function (on)
import Data.List     (deleteBy, find)

import Text.ParserCombinators.Parsec hiding ((<|>))

import qualified Network.WebSockets as WS
import qualified Data.Text          as T
import qualified Data.Text.IO       as T


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
    act (Msg cId msg) = sendToClient cId msg

-- | Set the MOTD, and notify any currently connected clients
announce :: T.Text -> Serv ()
announce msg = do
  setMotd msg
  clients <- askClients
  liftIO $ readMVar clients >>= mapM_ (flip writeChan msg . clientChan)

-- | Send a message to a single client
sendToClient :: ClientId -> T.Text -> Serv ()
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

-- | Server context monad
newtype Serv a = Serv { runServ :: ReaderT ServerState IO a }
                 deriving (Functor, Monad, MonadIO, MonadCatchIO)

-- | Read-only server state
data ServerState = ServerState { serverClients :: MVar [Client]
                               , serverMotd    :: MVar (Maybe T.Text)
                               , serverNextId  :: MVar ClientId
                               }

type ClientId = Integer

data Client = Client { clientId   :: ClientId
                     , clientConn :: WS.Connection
                     , clientChan :: Chan T.Text
                     }

-- | Run through both 'Serv' and 'ReaderT'
runServ' :: ServerState -> Serv a -> IO a
runServ' st fa = flip runReaderT st $ runServ fa

initState :: IO ServerState
initState = liftM3 ServerState
  (newMVar [])
  (newMVar Nothing)
  (newMVar 1)

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


-- | Server admin commands
data Command = SetMotd T.Text
             | Msg ClientId T.Text

parseCommand :: String -> Either ParseError Command
parseCommand = parse (parseSetMotd <|> parseMsgClient) "console"

parseSetMotd :: GenParser Char st Command
parseSetMotd =
  SetMotd . T.pack <$> (string "SETMOTD" *> spaces *> many1 anyChar)

parseMsgClient :: GenParser Char st Command
parseMsgClient =
  Msg <$> (string "MSG" *> spaces *> parseClientId)
      <*> fmap T.pack (spaces *> many1 anyChar)

  where
    parseClientId = read <$> many1 digit

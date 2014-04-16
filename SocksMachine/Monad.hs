{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module SocksMachine.Monad where

import Control.Applicative        (Applicative)
import Control.Concurrent
import Control.Monad              (liftM3, void)
import Control.Monad.Base         (MonadBase)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Control

import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)

import qualified Data.Text          as T
import qualified Network.WebSockets as WS


-- | Server context monad
newtype Serv a = Serv { runServ :: ReaderT ServerState IO a }
                 deriving ( Functor
                          , Applicative
                          , Monad
                          , MonadIO
                          , MonadCatchIO
                          , MonadBase IO
                          )

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

instance MonadBaseControl IO Serv where
  newtype StM Serv a = StServ { unStServ :: StM (ReaderT ServerState IO) a }
  liftBaseWith action = Serv $ liftBaseWith $ \run -> action $ fmap StServ . run . runServ
  restoreM = Serv . restoreM . unStServ

-- | Run through both 'Serv' and 'ReaderT'
runServ' :: ServerState -> Serv a -> IO a
runServ' st = flip runReaderT st . runServ

initState :: IO ServerState
initState = liftM3 ServerState
  (newMVar [])
  (newMVar Nothing)
  (newMVar 1)

askClients :: Serv (MVar [Client])
askClients = Serv $ asks serverClients

askMotd :: Serv (MVar (Maybe T.Text))
askMotd = Serv $ asks serverMotd

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

modifyClients :: ([Client] -> [Client]) -> Serv ()
modifyClients f = do
  clients <- askClients
  liftIO $ modifyMVar_ clients (return . f)


liftBaseOpDiscard :: MonadBaseControl b m
                  => ((a -> b ()) -> b α)
                  ->  (a -> m ()) -> m α
liftBaseOpDiscard f g = liftBaseWith $ \runInBase -> f $ void . runInBase . g

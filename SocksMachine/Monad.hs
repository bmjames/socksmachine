{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module SocksMachine.Monad where

import Control.Concurrent
import Control.Monad              (liftM3)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)

import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)

import qualified Data.Text          as T
import qualified Network.WebSockets as WS


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

askClients :: Serv (MVar [Client])
askClients = Serv $ asks serverClients

askMotd :: Serv (MVar (Maybe T.Text))
askMotd = Serv $ asks serverMotd

modifyClients :: ([Client] -> [Client]) -> Serv ()
modifyClients f = do
  clients <- askClients
  liftIO $ modifyMVar_ clients (return . f)

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception  (bracket)
import Control.Monad      (forever)

import Data.Function      (on)
import Data.List          (deleteBy)
import Data.Text          (Text)
import Data.UUID          (UUID)

import System.UUID.V4     (uuid)

import qualified Network.WebSockets as WS


main :: IO ()
main = do
  st <- initState
  WS.runServer "0.0.0.0" 9160 $ serve st

serve :: ServerState -> WS.PendingConnection -> IO ()
serve st pending = do
  conn <- WS.acceptRequest pending
  bracket
    (registerClient st conn)
    (unregisterClient st)
    (forever . talk)

talk :: Client -> IO ()
talk (Client _ conn) = receiveTextData conn >>= WS.sendTextData conn


-- Server state

data Client = Client { clientId   :: UUID
                     , clientConn :: WS.Connection
                     }

data ServerState = ServerState { clients :: MVar [Client] }

initState :: IO ServerState
initState = fmap ServerState $ newMVar []

registerClient :: ServerState -> WS.Connection -> IO Client
registerClient st conn = do
  cid <- uuid
  let client = Client cid conn
  modifyMVar_ (clients st) (return . (client :))
  putStrLn $ "Client registered: " ++ show cid
  return client

unregisterClient :: ServerState -> Client -> IO ()
unregisterClient st client = do
  modifyMVar_ (clients st) (return . deleteBy ((==) `on` clientId) client)
  putStrLn $ "Client unregistered: " ++ show (clientId client)


-- WS utils

receiveTextData :: WS.Connection -> IO Text
receiveTextData = WS.receiveData

module Main where

import Prelude hiding (mapM_)

import SocksMachine.Server
import SocksMachine.Monad

import Control.Concurrent          (forkIO)
import Control.Monad.Trans.Control (liftBaseDiscard)


-- | Start a server, read lines from stdin and broadcast to all clients.
main :: IO ()
main = do
  st <- initState
  runServ' st $ do
    liftBaseDiscard forkIO $ serverMain "0.0.0.0" 9160
    serverConsoleMain

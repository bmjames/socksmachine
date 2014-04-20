module Main where

import Prelude hiding (mapM_)

import SocksMachine.Server
import SocksMachine.Monad

import Control.Concurrent.Async.Lifted (concurrently)
import Control.Monad                   (void)


-- | Start a server, read lines from stdin and broadcast to all clients.
main :: IO ()
main = do
  st <- initState
  runServ' st . void $ concurrently
   (serverMain "0.0.0.0" 9160)
   serverConsoleMain

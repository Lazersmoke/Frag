{-# LANGUAGE OverloadedStrings #-}

module Main where

import Purity.Util
import Purity.Data -- REPL import
import Purity.Network
import Purity.Control

import Control.Concurrent
import qualified Network.WebSockets as WS

main :: IO ()
main = do
  -- Generate fresh ServerState to store in the MVar
  state <- newMVar testServerState
  -- Start the server, feeding each connection handler the server state's MVar. Also throw away thread ID
  _ <- forkIO $ WS.runServer "0.0.0.0" 9160 $ onConnection state
  startMainLoop state

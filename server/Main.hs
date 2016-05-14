{-# LANGUAGE OverloadedStrings #-}

module Main where

import FragData
import FragNetwork
import FragControl

import Control.Concurrent
import qualified Network.WebSockets as WS

main :: IO ()
main = do
  -- Generate fresh ServerState to store in the MVar
  state <- newMVar freshServerState
  -- Start the server, feeding each connection handler the server state's MVar
  serverThread <- forkIO $ WS.runServer "0.0.0.0" 9160 $ onConnectionWrapper state
  mainWrapper state

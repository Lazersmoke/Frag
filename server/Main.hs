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
  -- Generate fresh command buffer to store in the MVar
  commandBuffer <- newMVar []
  -- Start the server, feeding each connection handler the server state's MVar. Also throw away thread ID
  _ <- forkIO $ WS.runServer "0.0.0.0" 9160 $ onConnection commandBuffer
  startMainLoop commandBuffer testServerState

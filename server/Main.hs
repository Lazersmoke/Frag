{-# LANGUAGE OverloadedStrings #-}

module Main where

import Purity.Server.Network
import Purity.Server.Control
import Purity.Server.World

import System.IO
import Control.Concurrent
import qualified Network.WebSockets as WS

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- make putStr actually work
  -- Generate fresh command buffer to store in the MVar
  commandBuffer <- newMVar []
  -- Start the server, feeding each connection handler the server state's MVar. Also throw away thread ID
  _ <- forkIO $ WS.runServer "0.0.0.0" 9160 $ onConnection commandBuffer
  putStr "Level Name: "
  fn <- getLine
  ss <- getServerState $ 
    case fn of
      -- Default to test level
      "" -> "server/testlevel.wpl"
      _ -> fn
  startMainLoop commandBuffer ss

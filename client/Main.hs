{-# LANGUAGE LambdaCase #-}
module Main where

import Purity.Client.Data
import Purity.Client.DefaultMode
import Purity.Client.Render
import Purity.Client.Network
import Purity.Client.Parse

-- REPL Imports
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Control.Concurrent

main :: IO ()
main = do
  print testWPL
  mvarRender <- newMVar (defaultMode :: Mode)
  _ <- forkIO $ beginRenderLoop mvarRender
  startDemo mvarRender
  --connectToServer "localhost" mvarRender


-- REPL Validation
_r :: GL.HasSetter a b => a -> b -> IO () 
_r = ($=)

_glfw :: GLFW.Window -> IO Bool
_glfw = GLFW.windowShouldClose

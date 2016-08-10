{-# LANGUAGE LambdaCase #-}
module Purity.Client.Render where

import Purity.Client.Data
import Purity.Client.Util 
import qualified Purity.Client.Demo as Demo
import qualified Purity.Client.Lobby as Lobby
import qualified Purity.Client.Game as Game

import Data.Access

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Control.Concurrent
import Control.Monad


-- Intended to be forked from main thread almost right away
beginRenderLoop :: MVar Mode -> IO ()
beginRenderLoop mvarRender = do
  maybeWin <- initialize
  -- This is correct; it will nullPtr if it is nothing, but we can still terminate
  GLFW.makeContextCurrent maybeWin
  case maybeWin of
    Nothing -> do
      plog Error "No OpenGL Window Created"
      GLFW.terminate
    Just win -> do
      GLFW.setWindowCloseCallback win (Just GLFW.destroyWindow)--I know how much you like explosions, Jimmy!
      GLFW.setKeyCallback win (Just $ keyboardCallback mvarRender) -- I pressa da button yass vary mucuh
      plog Log "OpenGL Window Created"
      renderLoop (draw mvarRender) win
      GLFW.destroyWindow win
      GLFW.terminate

renderLoop :: (GLFW.Window -> IO ()) -> GLFW.Window -> IO ()
renderLoop drawFunc win = do
  close <- GLFW.windowShouldClose win 
  unless close $ do
    drawFunc win
    GLFW.swapBuffers win
    GLFW.pollEvents
    renderLoop drawFunc win

draw :: MVar Mode -> GLFW.Window -> IO ()
draw mvarRender win = grab DrawFunction <$> readMVar mvarRender >>= ($ win)
      
initialize :: IO (Maybe GLFW.Window)
initialize = do
  plog Log "Initializing OpenGL"
  -- TODO: Handle the output instead of black holing it
  GLFW.init
  -- GLFW.getVersion >>= print
  GLFW.setErrorCallback (Just breakTheGlass) -- Jimmy! Just do it!
  GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
  GLFW.createWindow
    640 -- width
    480 -- height
    "Purity Client" -- Title
    Nothing -- Monitor (no monitor yet)
    Nothing -- Window (no monitor yet)

breakTheGlass :: GLFW.ErrorCallback
breakTheGlass err desc = plog Error $ show err ++ " | " ++ desc

keyboardCallback :: MVar Mode -> GLFW.KeyCallback
keyboardCallback mvarRender w k i ks m = grab KeyCallback <$> readMVar mvarRender >>= \mode -> mode w k i ks m


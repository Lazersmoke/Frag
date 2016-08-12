module Purity.Client.Lobby (lobbyMode) where

import Purity.Client.Data
import Purity.Client.DefaultMode
import Purity.Client.Util

import Control.Monad
import qualified Data.Text as T
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Network.WebSockets as WS

lobbyMode :: [LobbyEntry] -> WS.Connection -> Mode
lobbyMode entries conn = buildMode (render entries) (keyboard conn) defaultMouseButtonCallback

keyboard :: WS.Connection -> GLFW.KeyCallback
keyboard conn win key _ keyState _ = do
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) (GLFW.setWindowShouldClose win True)
  when (key == GLFW.Key'G && keyState == GLFW.KeyState'Pressed) (WS.sendTextData conn . T.pack $ "Ready")

render :: [LobbyEntry] -> GLFW.Window -> IO ()
render entries win = do
  (w,h) <- GLFW.getFramebufferSize win
  let entryHeight = fromIntegral w / fromIntegral (length entries)
  GL.cullFace $= Just GL.Back
  GL.depthFunc $= Just GL.Less
  GL.viewport $= (GL.Position 0 0,GL.Size (fromIntegral w) (fromIntegral h))
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (fromIntegral w) 0 (fromIntegral h) (negate 1) 1
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  GL.renderPrimitive GL.Quads . 
    forM_ [0..length entries] $ \entryNumber -> do
      let startHeight = fromIntegral h * (fromIntegral entryNumber :: Double) / fromIntegral (length entries)
      GL.color (GL.Color3 (getColor startHeight) 0 (getColor startHeight) :: GL.Color3 GL.GLdouble)
      rectangle2D (0,startHeight) (fromIntegral w, entryHeight + startHeight)
  return ()
  where
    -- For some reason, this lambda supresses the "assumed type" warning ¯\_(ツ)_/¯
    getColor :: Fractional b => Double -> b
    getColor = realToFrac . flip mod 255 . (\x -> floor x :: Integer)


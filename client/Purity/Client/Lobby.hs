module Purity.Client.Lobby where

import Purity.Client.Data

import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

lobbyMode :: Mode
lobbyMode = buildMode (Lobby []) render keyboard mouse

keyboard :: GLFW.KeyCallback
keyboard win key _ keyState _ = when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) (GLFW.setWindowShouldClose win True)

render :: [LobbyEntry] -> GLFW.Window -> IO ()
render entries win = do
  (w,h) <- GLFW.getFramebufferSize win
  GL.cullFace $= Just GL.Back
  GL.depthFunc $= Just GL.Less
  GL.viewport $= (GL.Position 0 0,GL.Size (fromIntegral w) (fromIntegral h))
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]
  let hPerEntry = fromIntegral w / fromIntegral (length entries)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (fromIntegral w) 0 (fromIntegral h) (negate 1) 1
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  GL.renderPrimitive GL.Quads . 
    forM_ [0..(length entries + 1)] $ \entryNumber -> do
      let fEntry = fromIntegral entryNumber :: Double
      let currentPart = fEntry / fromIntegral (length entries + 1)
      let nextPart = (fEntry + 1) / fromIntegral (length entries + 1)
      GL.color (GL.Color3 (realToFrac currentPart * 255) 0 (realToFrac currentPart * 255) :: GL.Color3 GL.GLdouble)
      GL.vertex (GL.Vertex2 0 (currentPart * fromIntegral h) :: GL.Vertex2 GL.GLdouble)
      GL.vertex (GL.Vertex2 (fromIntegral w) (currentPart * fromIntegral h) :: GL.Vertex2 GL.GLdouble)
      GL.vertex (GL.Vertex2 (fromIntegral w) (nextPart * fromIntegral h) :: GL.Vertex2 GL.GLdouble)
      GL.vertex (GL.Vertex2 0 (nextPart * fromIntegral h) :: GL.Vertex2 GL.GLdouble)
  return ()


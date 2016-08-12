module Purity.Client.Game (gameMode) where

import Purity.Client.Data
import Purity.Client.DefaultMode
import Purity.Client.Util

import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

gameMode :: [Object] -> [(String,Object)] -> [RenderPlane] -> Mode
gameMode objects players planes = buildMode
  (render planes)
  defaultKeyboardCallback
  defaultMouseButtonCallback

render :: [RenderPlane] -> GLFW.Window -> IO ()
render planes win = do
  (w,h) <- GLFW.getFramebufferSize win
  GL.cullFace $= Just GL.Back
  GL.depthFunc $= Just GL.Less
  GL.viewport $= (GL.Position 0 0,GL.Size (fromIntegral w) (fromIntegral h))
  let ratio = fromIntegral w / fromIntegral h
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 1.5 ratio 0.1 100
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

  GL.renderPrimitive GL.Triangles $ forM_ planes renderPlane

module Purity.Client.Demo where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

render :: GLFW.Window -> IO ()
render win = do
  (w,h) <- GLFW.getFramebufferSize win
  GL.cullFace $= Just GL.Back
  GL.depthFunc $= Just GL.Less
  GL.viewport $= (GL.Position 0 0,GL.Size (fromIntegral w) (fromIntegral h))
  let ratio = fromIntegral w / fromIntegral h
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0) 
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  Just t <- GLFW.getTime
  GL.rotate (realToFrac t * 50) (GL.Vector3 0 0 1 :: GL.Vector3 GL.GLdouble)

  GL.renderPrimitive GL.Triangles $ do
    GL.color (GL.Color3 1 0 0 :: GL.Color3 GL.GLdouble)
    GL.vertex (GL.Vertex3 (negate 0.6) (negate 0.4) 0 :: GL.Vertex3 GL.GLdouble)
    GL.color (GL.Color3 0 1 0 :: GL.Color3 GL.GLdouble)
    GL.vertex (GL.Vertex3 0.6 (negate 0.4) 0 :: GL.Vertex3 GL.GLdouble)
    GL.color (GL.Color3 0 0 1 :: GL.Color3 GL.GLdouble)
    GL.vertex (GL.Vertex3 0 0.6 0 :: GL.Vertex3 GL.GLdouble)



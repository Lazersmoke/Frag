module Purity.Client.Game (gameMode) where

import Purity.Client.Data
import Purity.Client.DefaultMode
import Purity.Client.Util

import Control.Monad
import Data.Access
import Data.IORef
import qualified Data.Text as T
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import qualified Network.WebSockets as WS

gameMode :: IORef (Double,Double) -> WS.Connection -> [Object] -> [(String,Object)] -> [RenderPlane] -> Mode
gameMode cursorRef conn _ players planes = buildMode
  (render planes (snd . head $ players))
  (keyboard conn)
  defaultMouseButtonCallback
  (mousePos cursorRef conn)

render :: [RenderPlane] -> Object -> Renderer
render planes viewFrom _ win = do
  (w,h) <- GLFW.getFramebufferSize win
  GL.depthFunc $= Just GL.Less
  GL.frontFace $= GL.CCW
  GL.viewport $= (GL.Position 0 0,GL.Size (fromIntegral w) (fromIntegral h))
  let ratio = fromIntegral w / fromIntegral h
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  let posV = Position ~>> viewFrom
  let lookV = FacingDirection ~>> viewFrom
  let rightV = normalizeVector $ crossProduct lookV (Vector 0 1 0)
  let upV = normalizeVector $ crossProduct rightV lookV
  putStr "posV: "
  print posV
  putStr "lookV: "
  print lookV
  putStr "rightV: "
  print rightV
  GL.lookAt (toGLVert $ Vector 0 1 0 + posV) (toGLVert $ Vector 0 1 0 + posV + lookV) (toGLVec upV)
  GL.perspective 150.0 ratio 0.1 100
  --plog Log $ "Looking from " ++ show (Vector 0 1 0 + posV) ++ " to " ++ show (Vector 0 1 0 + posV + lookV) ++ " and up is " ++ show upV
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

  GL.color (GL.Color3 0 200 0 :: GL.Color3 GL.GLdouble)

  print planes

  GL.renderPrimitive GL.Triangles $ forM_ planes renderPlane

keyboard :: WS.Connection -> GLFW.KeyCallback
keyboard conn win key _ keyState _ = do
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) (GLFW.setWindowShouldClose win True)
  let 
    bindings = 
      [("forward", GLFW.Key'W)
      ,("right", GLFW.Key'A)
      ,("back", GLFW.Key'S)
      ,("left", GLFW.Key'D)
      ]
  forM_ bindings $ \(mess,keybind) -> do
    when (key == keybind && keyState == GLFW.KeyState'Pressed) (send $ '+':mess)
    when (key == keybind && keyState == GLFW.KeyState'Released) (send $ '-':mess)
  when (key == GLFW.Key'I && keyState == GLFW.KeyState'Pressed) (send "look -1 -1")
  where
    send = WS.sendTextData conn . T.pack

mousePos :: IORef (Double,Double) -> WS.Connection -> GLFW.CursorPosCallback
mousePos lastPos conn _ x y = do
  (lx,ly) <- readIORef lastPos
  writeIORef lastPos (x,y)
  WS.sendTextData conn . T.pack $ "look " ++ show (x-lx) ++ " " ++ show (y-ly)

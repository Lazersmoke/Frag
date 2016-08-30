module Purity.Client.Demo where

import Purity.Client.Data
import Purity.Client.Util
import Purity.Client.DefaultMode

import Control.Monad
import Data.Access
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLRaw
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

demoMode :: Mode
demoMode = buildMode
  render
  defaultKeyboardCallback
  defaultMouseButtonCallback
  defaultCursorPosCallback

--fov
perspectiveMatrix :: Float -> Float -> Float -> Float -> V.Vector GL.GLfloat
perspectiveMatrix fov ratio near far = V.fromList (
  [x,0,0,0
  ,0,y,0,0
  ,0,0,z,w
  ,0,0,-1,0
  ] :: [GL.GLfloat])
  where
    q = tan $ fov / 2
    x = 1 / (ratio * q)
    y = 1 / q
    z = -(far + near) / (far - near)
    w = -(2 * far * near) / (far - near)

-- sketch AF REDO THIS PART RIGHT HERE VVVVVV
-- https://hackage.haskell.org/package/linear-1.13/docs/src/Linear-Perspective.html#lookAt
lookAt :: Vector -> Vector -> Vector -> V.Vector GL.GLfloat
lookAt eye center up = V.fromList
  (map realToFrac 
  [X ~>> xa, X ~>> ya, X ~>> za, 0
  ,Y ~>> xa, Y ~>> ya, Y ~>> za, 0
  ,Z ~>> xa, Z ~>> ya, Z ~>> za, 0
  ,xd      , yd      , zd      , 1] :: [Float])
  where
    za = normalizeVector $ center - eye
    xa = normalizeVector $ crossProduct up za
    ya = crossProduct za xa
    xd = -dotProduct xa eye
    yd = -dotProduct ya eye
    zd = -dotProduct za eye

render :: Renderer
render program win = do
  (w,h) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  let ratio = fromIntegral w / fromIntegral h
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]

  GL.currentProgram $= Just program
  let 
    perspective = perspectiveMatrix 45 ratio 0.1 100.0
    lookMat = lookAt (Vector 4 3 (-3)) (Vector 0 0 0) (Vector 0 1 0)

  withAttribute 0 testVerts $ 
    withAttribute 1 testColors $ do
      transferMatrices program [("projection",perspective),("look",lookMat)]
      GL.drawArrays GL.Triangles 0 36

withAttribute :: GL.GLuint -> V.Vector GL.GLfloat -> IO () -> IO ()
withAttribute attIndex arr action = do
  -- Enable VAO
  GL.vertexAttribArray (GL.AttribLocation attIndex) $= GL.Enabled
  -- Fill VAO
  V.unsafeWith arr $ \ptr ->
    GL.vertexAttribPointer (GL.AttribLocation attIndex) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
  action
  GL.vertexAttribArray (GL.AttribLocation attIndex) $= GL.Disabled


transferMatrices :: GL.Program -> [(String,V.Vector GL.GLfloat)] -> IO ()
transferMatrices program xs = 
  forM_ xs $ \(uniName,inputMatrix) -> do
    -- Get uniform for projection matrix
    GL.UniformLocation loc <- GL.get (GL.uniformLocation program uniName)
    -- Fill projection matrix
    V.unsafeWith inputMatrix $ \ptr -> GLRaw.glUniformMatrix4fv loc 1 0 ptr


testColors :: V.Vector GL.GLfloat
testColors = V.fromList
  [0.583,0.771,0.014
  ,0.609,0.115,0.436
  ,0.327,0.483,0.844
  ,0.822,0.569,0.201
  ,0.435,0.602,0.223
  ,0.310,0.747,0.185
  ,0.597,0.770,0.761
  ,0.559,0.436,0.730
  ,0.359,0.583,0.152
  ,0.483,0.596,0.789
  ,0.559,0.861,0.639
  ,0.195,0.548,0.859
  ,0.014,0.184,0.576
  ,0.771,0.328,0.970
  ,0.406,0.615,0.116
  ,0.676,0.977,0.133
  ,0.971,0.572,0.833
  ,0.140,0.616,0.489
  ,0.997,0.513,0.064
  ,0.945,0.719,0.592
  ,0.543,0.021,0.978
  ,0.279,0.317,0.505
  ,0.167,0.620,0.077
  ,0.347,0.857,0.137
  ,0.055,0.953,0.042
  ,0.714,0.505,0.345
  ,0.783,0.290,0.734
  ,0.722,0.645,0.174
  ,0.302,0.455,0.848
  ,0.225,0.587,0.040
  ,0.517,0.713,0.338
  ,0.053,0.959,0.120
  ,0.393,0.621,0.362
  ,0.673,0.211,0.457
  ,0.820,0.883,0.371
  ,0.982,0.099,0.879]

testVerts :: V.Vector GL.GLfloat
testVerts = V.fromList 
  [-1,-1,-1
  ,-1,-1, 1
  ,-1, 1, 1
  , 1, 1,-1
  ,-1,-1,-1
  ,-1, 1,-1
  , 1,-1, 1
  ,-1,-1,-1
  , 1,-1,-1
  , 1, 1,-1
  , 1,-1,-1
  ,-1,-1,-1
  ,-1,-1,-1
  ,-1, 1, 1
  ,-1, 1,-1
  , 1,-1, 1
  ,-1,-1, 1
  ,-1,-1,-1
  ,-1, 1, 1
  ,-1,-1, 1
  , 1,-1, 1
  , 1, 1, 1
  , 1,-1,-1
  , 1, 1,-1
  , 1,-1,-1
  , 1, 1, 1
  , 1,-1, 1
  , 1, 1, 1
  , 1, 1,-1
  ,-1, 1,-1
  , 1, 1, 1
  ,-1, 1,-1
  ,-1, 1, 1
  , 1, 1, 1
  ,-1, 1, 1
  , 1,-1, 1]


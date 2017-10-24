{-# LANGUAGE LambdaCase #-}
module Purity where

import qualified Numeric.LinearAlgebra as Mat
import qualified Numeric.LinearAlgebra.Devel as MatDevel
import Graphics.GL.Core45
import qualified Graphics.UI.GLFW as GLFW
import Foreign
import Foreign.C.String
import Control.Monad
import Codec.Wavefront


data OpenGLInfo = OpenGLInfo
  {vertexBuffer :: GLuint
  ,indexBuffer :: GLuint
  ,shaderProgram :: GLuint
  ,vertexMatricies :: [(String,GLint,Mat.Matrix Float)]
  }

purityMain :: IO ()
purityMain = do
  putStrLn . ("Using GLFW version " ++) . show =<< GLFW.getVersion
  putStr "Initializing GLFW... "
  GLFW.init >>= \case
    True -> do
      putStrLn "Success!"
      putStrLn "Hinting GLFW: {Samples -> 4, OpenGL -> Forward Compatible 3.3 Core}"
      GLFW.windowHint $ GLFW.WindowHint'Samples 4
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      putStr "Creating 1024x768 window... "
      GLFW.createWindow 1024 768 "Purity" Nothing Nothing >>= \case
        Nothing -> do
          putStrLn "Failure!"
          GLFW.terminate
        Just theWindow -> do
          putStrLn "Success!"
          putStrLn "Making the window the current context..."
          GLFW.makeContextCurrent (Just theWindow)
          putStrLn "Setting input mode so we can get events..."
          GLFW.setStickyKeysInputMode theWindow GLFW.StickyKeysInputMode'Enabled
          putStrLn "Initializing OpenGL..."
          openGLInfo <- initGL
          putStrLn "Entering render loop..."
          renderLoop openGLInfo theWindow 1
    False -> putStrLn "Failure!"

renderLoop :: OpenGLInfo -> GLFW.Window -> Double -> IO ()
renderLoop openGLInfo theWindow t = do
  putStrLn "Drawing..."
  draw openGLInfo
  let f = Mat.cmap realToFrac $ lookAt (Mat.vector [4,3,3]) (Mat.vector [0,0,0]) (Mat.vector [0,1,0])
  putStrLn $ "Latest cos(t)=" ++ show (cos t)
  putStrLn "Latest view matrix:"
  print f
  let newInfo = openGLInfo {vertexMatricies = [vertexMatricies openGLInfo !! 0, (\(a,b,_) -> (a,b,f)) $ vertexMatricies openGLInfo !! 1,vertexMatricies openGLInfo !! 2]}
  putStrLn "Swapping buffers..."
  GLFW.swapBuffers theWindow
  putStrLn "Polling events..."
  GLFW.pollEvents
  GLFW.windowShouldClose theWindow >>= \case
    True -> do
      putStrLn "We've been told to close the window, doing so..."
      GLFW.destroyWindow theWindow
    False -> GLFW.getKey theWindow GLFW.Key'Escape >>= \case
      GLFW.KeyState'Pressed -> do
        putStrLn "Escape key was pressed, closing window"
        GLFW.destroyWindow theWindow
      _ -> renderLoop newInfo theWindow (t + 0.01)
  

initGL :: IO OpenGLInfo
initGL = do
  putStrLn "Setting clear color to blue..."
  glClearColor 0 0 4 0

  (vao,vbo,ibo) <- alloca $ \namePtr -> do
    putStr "Generating VAO Name..."
    glGenVertexArrays 1 namePtr
    vao <- peek namePtr
    putStrLn $ show vao

    putStr "Generating VBO Name..."
    glGenBuffers 1 namePtr
    vbo <- peek namePtr
    putStrLn $ show vbo

    putStr "Generating Index Buffer Name..."
    glGenBuffers 1 namePtr
    ibo <- peek namePtr
    putStrLn $ show ibo
    pure (vao,vbo,ibo)

  putStrLn "Binding VAO..."
  glBindVertexArray vao
  putStrLn "Buffering vertex attributes..."

  putStrLn "Binding VBO..."
  glBindBuffer GL_ARRAY_BUFFER vbo
  putStrLn "Buffering vertex positions..."
  let vertexData = [-1,-1,0, 1,-1,0, 0,1,0] :: [GLfloat]
  withArray vertexData $ \vertexDataArray -> do
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ length vertexData * sizeOf (undefined :: GLfloat)) vertexDataArray GL_STATIC_DRAW

  putStrLn "Binding index buffer..."
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
  putStrLn "Buffering indicies..."
  let indexData = [0,1,2] :: [GLushort]
  withArray indexData $ \indexDataArray -> do
    glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ length indexData * sizeOf (undefined :: GLushort)) indexDataArray GL_STATIC_DRAW

  putStrLn "Loading Vertex Shader..."
  vShader <- loadShader GL_VERTEX_SHADER "vertexShader"
  putStrLn "Loading Fragment Shader..."
  fShader <- loadShader GL_FRAGMENT_SHADER "fragmentShader"

  putStrLn "Creating program..."
  programId <- glCreateProgram
  putStrLn "Attaching Vertex Shader..."
  glAttachShader programId vShader
  putStrLn "Attaching Fragment Shader..."
  glAttachShader programId fShader
  putStrLn "Linking program..."
  glLinkProgram programId

  putStr "Checking program..."
  (status,logLength) <- alloca $ \result -> do
    glGetProgramiv programId GL_LINK_STATUS result
    status <- peek result
    glGetProgramiv programId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)

  putStrLn $ "link status was " ++ show status
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetProgramInfoLog programId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  putStrLn $ "Program linking log:"
  putStrLn logMessage

  putStrLn "Detaching Vertex Shader..."
  glDetachShader programId vShader
  putStrLn "Detaching Fragment Shader..."
  glDetachShader programId fShader

  putStrLn "Deleting Vertex Shader..."
  glDeleteShader vShader
  putStrLn "Deleting Fragment Shader..."
  glDeleteShader fShader

  [modelId,viewId,projectionId] <- mapM (flip withCString (glGetUniformLocation programId)) ["Model","View","Projection"]
  let modelMat = Mat.ident 4
  let viewMat = Mat.cmap realToFrac $ lookAt (Mat.vector [4,3,3]) (Mat.vector [0,0,0]) (Mat.vector [0,1,0])
  let projectionMat = Mat.cmap realToFrac $ perspective 45 (4.0/3) 0.1 100

  pure $ OpenGLInfo {vertexBuffer = vbo,indexBuffer = ibo,shaderProgram = programId,vertexMatricies = [("Model",modelId,modelMat),("View",viewId,viewMat),("Projection",projectionId,projectionMat)]}

--mvp :: Mat.Matrix Float
--mvp = Mat.cmap fromIntegral $ perspective 45 (4/3) 0.1 100 Mat.<> lookAt (Mat.vector [4,3,3]) (Mat.vector [0,0,0]) (Mat.vector [0,1,0]) Mat.<> Mat.ident 4

stdOrtho :: Mat.Matrix Double
stdOrtho = Mat.fromLists
  [[1,0,0,0]
  ,[0,1,0,0]
  ,[0,0,-1,0]
  ,[0,0,0,1]
  ]

perspective :: Double -> Double -> Double -> Double -> Mat.Matrix Double
perspective fovy aspect zNear zFar = Mat.fromLists
  [[x,0,0,0]
  ,[0,f,0,0]
  ,[0,0,z,d]
  ,[0,0,-1,0]
  ]
  where
    x = 1/(tanHalfFovy * aspect)
    f = 1/tanHalfFovy
    z = - (zFar + zNear)/(zFar - zNear)
    d = - (2 * zFar * zNear)/(zFar - zNear)
    tanHalfFovy = tan $ fovy * pi/360

lookAt :: Mat.Vector Mat.R -> Mat.Vector Mat.R -> Mat.Vector Mat.R -> Mat.Matrix Mat.R
lookAt eye center up = (viewToWorld3 Mat.||| transWorldCoords) Mat.=== Mat.row [0,0,0,1]
  where
    forward = Mat.normalize (center - eye)
    sideways = Mat.cross forward (Mat.normalize up)
    cameraUp = Mat.cross (Mat.normalize sideways) forward
    worldToView3 = Mat.fromColumns [sideways,cameraUp,negate forward]
    viewToWorld3 = Mat.tr' worldToView3
    transWorldCoords = viewToWorld3 Mat.<> Mat.asColumn (negate eye)

loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderType shaderPath = do
  putStrLn $ "Creating " ++ shaderPath ++ "..."
  shaderId <- glCreateShader shaderType
  putStrLn $ "Loading " ++ shaderPath ++ " source..."
  shaderSource <- readFile shaderPath 
  withCString shaderSource (\cs -> new cs >>= \s -> glShaderSource shaderId 1 s nullPtr)
  putStrLn $ "Compiling " ++ shaderPath ++ "..."
  glCompileShader shaderId
  putStr $ "Checking " ++ shaderPath ++ "..."
  (status,logLength) <- alloca $ \result -> do
    glGetShaderiv shaderId GL_COMPILE_STATUS result
    status <- peek result
    glGetShaderiv shaderId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)
  putStrLn $ "compile status was " ++ show status
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetShaderInfoLog shaderId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  putStrLn $ shaderPath ++ " compilation log:"
  putStrLn logMessage
  pure shaderId

draw :: OpenGLInfo -> IO ()
draw openGLInfo = do
  putStrLn "Clearing screen..."
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  putStrLn "Using shader program..."
  glUseProgram (shaderProgram openGLInfo)
  forM_ (vertexMatricies openGLInfo) $ \(matName,matId,mat) -> do
    let (fPtr,_,_) = MatDevel.unsafeToForeignPtr . Mat.flatten $ mat
    putStrLn $ "Sending " ++ matName ++ " matrix..."
    print mat
    withForeignPtr fPtr $ glUniformMatrix4fv matId 1 GL_TRUE
  {-
  putStrLn "Combined matrix:"
  let mvp = foldr (Mat.<>) (Mat.ident 4) (reverse . map (\(_,_,m) -> m) . vertexMatricies $ openGLInfo)
  print mvp
  -}

  putStrLn "Enabling vertex position attributes..."
  glEnableVertexAttribArray 0
  putStrLn "Binding VBO..."
  glBindBuffer GL_ARRAY_BUFFER (vertexBuffer openGLInfo)
  putStrLn "Defining vertex attribute attributes..."
  -- Attr 0, 3 floats, don't normalize, no stride (skipping), no offset
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

  putStrLn "Binding index buffer..."
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER (indexBuffer openGLInfo)

  putStrLn "Drawing triangles..."
  glDrawElements GL_TRIANGLES 3 GL_UNSIGNED_SHORT nullPtr
  putStrLn "Disabling vertex position attributes..."
  glDisableVertexAttribArray 0

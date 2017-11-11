{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Yay
module Purity where

import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import Graphics.GL.Core45
import qualified Graphics.UI.GLFW as GLFW
import Foreign
import Foreign.C.String
import Control.Monad
import qualified Codec.Wavefront as Wavefront
import Data.Void
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TQueue as TQ
import System.IO.Unsafe (unsafePerformIO)
import qualified Linear as Linear
import Control.Lens
import System.Exit
import qualified Codec.Picture as Juicy

import Purity.Physics

-- | All the context that must be passed from OpenGL initialization to the renderer
data OpenGLInfo = OpenGLInfo
  {attributeBuffers :: [VertexAttribute]
  ,indexBuffer :: GLuint
  ,indexCount :: GLsizei
  ,shaderProgram :: GLuint
  ,modelMatrixId :: GLint
  ,viewMatrixId :: GLint
  ,projectionMatrixId :: GLint
  ,lightLocationId :: GLint
  ,textureId :: GLuint
  ,textureSamplerId :: GLint
  }

-- | A prepared vertex attribute
data VertexAttribute = VertexAttribute
  {attributeName :: String
  ,attributeObjectName :: GLuint
  ,attributeIndex :: GLuint
  ,attributeSize :: GLint
  ,attributeType :: GLenum
  ,attributeNormalized :: GLboolean
  ,attributeStride :: GLsizei
  ,attributeOffset :: Ptr Void
  }

-- | A type encapsulating all state in purity
data PurityState = PurityState
  {_playerBody :: FreeBody Float
  ,_playerForward :: Linear.Quaternion Float -- Rotate 0 0 -1 to the player's view
  ,_frameTime :: Double
  ,_lightPosition :: Linear.V3 Float
  }
makeLenses ''PurityState

-- | Log a string asynchronously
logStr :: String -> IO ()
logStr = STM.atomically . TQ.writeTQueue globalLogger

-- | Log a string asynchronously and put a newline on it
logStrLn :: String -> IO ()
logStrLn = logStr . (++"\n")

-- | Make a string green
green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"

-- | Make a string red
red :: String -> String
red s = "\x1b[31m" ++ s ++ "\x1b[0m"

-- | The global logging queue
globalLogger :: TQ.TQueue String
globalLogger = unsafePerformIO TQ.newTQueueIO

-- | The main entry point
purityMain :: IO ()
purityMain = do
  _ <- Concurrent.forkIO (() <$ loggingThread)
  logStrLn . ("Using GLFW version " ++) . show =<< GLFW.getVersion
  logStr "Initializing GLFW... "
  GLFW.init >>= \case
    True -> do
      logStrLn $ green "Success!"
      logStrLn "Hinting GLFW: {Samples -> 4, OpenGL -> Forward Compatible 3.3 Core}"
      GLFW.windowHint $ GLFW.WindowHint'Samples 4
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      logStr "Creating fullscreen 1024x768 window... "
      let
        windowX = 1024
        windowY = 768
      GLFW.createWindow windowX windowY "Purity" Nothing Nothing >>= \case
        Nothing -> do
          logStrLn $ red "Failure!"
          GLFW.terminate
        Just theWindow -> do
          logStrLn $ green "Success!"
          logStrLn "Making the window the current context..."
          GLFW.makeContextCurrent (Just theWindow)
          logStrLn "Setting keyboard input mode so we can get events..."
          GLFW.setStickyKeysInputMode theWindow GLFW.StickyKeysInputMode'Enabled
          logStrLn "Setting mouse input mode so it doesn't move outside the window..."
          GLFW.setCursorInputMode theWindow GLFW.CursorInputMode'Disabled
          logStrLn "Polling events once to get things started..."
          GLFW.pollEvents
          logStrLn "Setting mouse to center screen..."
          GLFW.setCursorPos theWindow (fromIntegral windowX/2) (fromIntegral windowY/2)
          logStrLn "Initializing OpenGL..."
          openGLInfo <- initGL
          logStrLn "Entering render loop..."
          Just ft0 <- GLFW.getTime
          let
            bodyPosition = Linear.V3 0 0 0
          renderLoop PurityState{_lightPosition = Linear.V3 4 3 3,_playerBody = bodyAtRest bodyPosition, _playerForward = Linear.normalize $ Linear.axisAngle (negate bodyPosition) 0, _frameTime = ft0} openGLInfo theWindow
    False -> logStrLn "Failure!"

-- | A thread that logs things from the global logger forever
loggingThread :: IO Void
loggingThread = (STM.atomically (TQ.readTQueue globalLogger) >>= putStr) *> loggingThread

-- | A loop that renders the scene until the program ends
renderLoop :: PurityState -> OpenGLInfo -> GLFW.Window -> IO ()
renderLoop !state openGLInfo theWindow = do
  let 
    mMat = Linear.V4
      (Linear.V4 0.1 0 0 0)
      (Linear.V4 0 0.1 0 0)
      (Linear.V4 0 0 0.1 0)
      (Linear.V4 0 0 0 1)
    vMat = Linear.inv44 $ lookIn (state^.playerBody^.freeBodyPosition) (state^.playerForward)
    pMat = Linear.perspective (45 * pi/180) (4/3) 0.1 100
  -- Render this frame
  logStrLn "Drawing..."
  draw openGLInfo (mMat,vMat,pMat,state^.lightPosition)
  logStrLn "Swapping buffers..."
  GLFW.swapBuffers theWindow

  logStrLn "Polling events..."
  GLFW.pollEvents
  Just ft <- GLFW.getTime
  let dt = ft - state^.frameTime

  -- Move looking direction
  (cx,cy) <- GLFW.getCursorPos theWindow
  (wx,wy) <- GLFW.getWindowSize theWindow
  GLFW.setCursorPos theWindow (fromIntegral wx/2) (fromIntegral wy/2)
  let 
    forward = Linear.rotate (state^.playerForward) $ Linear.V3 0 0 (-1)
    right = Linear.normalize $ Linear.cross forward (Linear.rotate (state^.playerForward) $ Linear.V3 0 1 0)
    up = Linear.normalize $ Linear.cross right forward
    (dx,dy) = ((fromIntegral wx/2)-cx,(fromIntegral wy/2)-cy)
    mouseSpeed = dt * 0.01 -- radians/pixel
    (tx,ty) = (realToFrac $ dx * mouseSpeed,realToFrac $ dy * mouseSpeed) :: (Float,Float)
    yaw = Linear.axisAngle {-(Linear.V3 0 1 0)-} up tx
    -- Clamp so they can't go upside down
    currty = acos $ Linear.dot (Linear.V3 0 (-1) 0) forward
    ty' = max (min ty (pi - currty)) (-currty)
    pitch = Linear.axisAngle right ty'

  logStrLn $ "Read cursor at " ++ show (cx,cy) ++ " and noted the change was " ++ show (dx,dy) ++ " off from " ++ show (fromIntegral wx/2 :: Float,fromIntegral wy/2 :: Float) ++ " so used angles " ++ show (tx,ty)
  logStrLn $ "currty:\n" ++ show currty
  logStrLn $ "forward:\n" ++ show forward
  logStrLn $ "playerForward:\n" ++ show (state^.playerForward)
  logStrLn $ "ty:\n" ++ show ty
  logStrLn $ "ty':\n" ++ show ty'
  logStrLn $ "Yaw:\n" ++ show yaw
  logStrLn $ "Pitch:\n" ++ show pitch

  -- Move player
  moveVec <- sum . zipWith (\v b -> if b then v else Linear.V3 0 0 0) [forward,-right,-forward,right,up,-up] . fmap (==GLFW.KeyState'Pressed) <$> mapM (GLFW.getKey theWindow) [GLFW.Key'W,GLFW.Key'A,GLFW.Key'S,GLFW.Key'D,GLFW.Key'Q,GLFW.Key'Z]

  -- Get light key
  lPressed <- (==GLFW.KeyState'Pressed) <$> GLFW.getKey theWindow GLFW.Key'L

  let 
    moveSpeed = realToFrac dt * 0.9
    updateLight = if lPressed then lightPosition .~ (state^.playerBody^.freeBodyPosition) else id
    updatePosition = playerBody.freeBodyVelocity +~ moveSpeed Linear.*^ moveVec
    updateForward = playerForward %~ rotateView
    rotateView q = Linear.normalize $ pitch * yaw * q
    mu = 0.8
    friction = mu * negate (state^.playerBody.freeBodyVelocity)
    updatePhysics = playerBody %~ simulateFreeForce (realToFrac dt) ({-gravity +-} friction) --groundSurface
    state' = updateLight . updatePosition . updateForward . (frameTime .~ ft) . updatePhysics $ state

  logStrLn $ "Player body: " ++ show (state^.playerBody)
  logStrLn $ "Player forward: " ++ show (state^.playerForward)

  GLFW.windowShouldClose theWindow >>= \case
    True -> do
      logStrLn "We've been told to close the window, doing so..."
      GLFW.destroyWindow theWindow
    False -> GLFW.getKey theWindow GLFW.Key'Escape >>= \case
      GLFW.KeyState'Pressed -> do
        logStrLn "Escape key was pressed, closing window"
        GLFW.destroyWindow theWindow
      _ -> Concurrent.threadDelay 1000 *> if not hardBrake then renderLoop state' openGLInfo theWindow else logStrLn "Hard braking"

hardBrake :: Bool
hardBrake = False

bufferGLData :: forall a. (Show a,Storable a) => String -> GLuint -> GLenum -> [a] -> IO ()
bufferGLData name bufferName bufferType bufferData = do
  logStrLn $ name ++ " data is:"
  mapM_ logStrLn $ take 3 (map show bufferData) ++ ["..."] ++ drop (length bufferData - 3) (map show bufferData)
  logStrLn $ "Binding " ++ name ++ " buffer..."
  glBindBuffer bufferType bufferName
  logStrLn $ "Buffering " ++ name ++ " data..."
  withArray bufferData $ \bufferDataArray -> do
    glBufferData bufferType (fromIntegral $ length bufferData * sizeOf (undefined :: a)) bufferDataArray GL_STATIC_DRAW


-- | Initialize OpenGL, returning the rendering context
initGL :: IO OpenGLInfo
initGL = do
  logStrLn "Setting clear color to blue..."
  glClearColor 0 0 4 0

  logStrLn "Enabling depth testing..."
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS

  logStrLn "Enabling back face culling..."
  glEnable GL_CULL_FACE

  logStrLn "Generating Vertex Array Object Name..."
  vao <- alloca $ \namePtr -> do
    glGenVertexArrays 1 namePtr
    peek namePtr
  logStrLn $ "Vertex Array Object: " ++ show vao

  logStrLn "Generating Vertex Buffer Names..."
  [vbo,tbo,nbo,ibo] <- alloca $ \namePtr -> do
    glGenBuffers 4 namePtr
    peekArray 4 namePtr
  logStrLn $ "Vertex buffer: " ++ show vbo
  logStrLn $ "Texture coordinate buffer: " ++ show tbo
  logStrLn $ "Normal buffer: " ++ show nbo
  logStrLn $ "Index buffer: " ++ show ibo

  logStrLn "Generating texture name..."
  textureId' <- alloca $ \namePtr -> glGenTextures 1 namePtr *> peek namePtr
  logStrLn "Binding texture..."
  glBindTexture GL_TEXTURE_2D textureId'

  logStrLn "Loading texture juicily from file..."
  Right dynImage <- Juicy.readPng "test.png"
  let testTexture = Juicy.convertRGB8 dynImage

  logStrLn "Sending texture to GL..."
  Storable.unsafeWith (Juicy.imageData testTexture) $ glTexImage2D
    GL_TEXTURE_2D
    0 -- Mipmap level (0 full res)
    (fromIntegral GL_RGB) -- internal format
    (fromIntegral $ Juicy.imageWidth testTexture)
    (fromIntegral $ Juicy.imageHeight testTexture)
    0 -- This is 0 because OpenGL. L
    GL_RGB -- stored format
    GL_UNSIGNED_BYTE -- size of color component

  logStrLn "Configuring mipmaps..."
  logStrLn "  Magnify using linear filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
  logStrLn "  Minify using linear blending and filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
  logStrLn "Generating mipmaps..."
  glGenerateMipmap GL_TEXTURE_2D

  logStrLn "Binding VAO..."
  glBindVertexArray vao
  logStrLn "Buffering vertex attributes..."

  logStrLn "Loading model..."
  model <- Wavefront.fromFile "normalTeapot.obj" >>= \case
    Right m -> pure m
    Left e -> error e

  let vertexData = fmap (\(Wavefront.Location x y z _w) -> Linear.V3 x y z) . Vector.toList $ Wavefront.objLocations model
  bufferGLData "position" vbo GL_ARRAY_BUFFER (vertexData :: [Linear.V3 GLfloat])

  let textureData = fmap (\(Wavefront.TexCoord r s _t) -> Linear.V2 r s) . Vector.toList . Wavefront.objTexCoords $ model
  bufferGLData "texture coordinate" tbo GL_ARRAY_BUFFER (textureData :: [Linear.V2 GLfloat])

  let normalData = fmap (\(Wavefront.Normal x y z) -> Linear.V3 x y z) . Vector.toList . Wavefront.objNormals $ model
  bufferGLData "normal" nbo GL_ARRAY_BUFFER (normalData :: [Linear.V3 GLfloat])

  let indexData = fmap (fmap (fromIntegral . subtract 1 . Wavefront.faceLocIndex) . (\(Wavefront.Face a b c _fs) -> Linear.V3 a b c) . Wavefront.elValue) . Vector.toList . Wavefront.objFaces $ model
  bufferGLData "index" ibo GL_ELEMENT_ARRAY_BUFFER (indexData :: [Linear.V3 GLushort])

  logStrLn "Loading Vertex Shader..."
  vShader <- loadShader GL_VERTEX_SHADER "vertexShader"
  logStrLn "Loading Fragment Shader..."
  fShader <- loadShader GL_FRAGMENT_SHADER "fragmentShader"

  logStrLn "Creating program..."
  programId <- glCreateProgram

  forM_ [("Vertex",vShader),("Fragment",fShader)] $ \(name,shader) -> do
    logStrLn $ "Attaching " ++ name ++ " Shader..."
    glAttachShader programId shader

  logStrLn "Linking program..."
  glLinkProgram programId
  logStr "Checking program..."
  (status,logLength) <- alloca $ \result -> do
    glGetProgramiv programId GL_LINK_STATUS result
    status <- peek result
    glGetProgramiv programId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)
  let success = fromIntegral status == GL_TRUE
  logStrLn $ "link status was " ++ (if success then green else red) (show status)
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetProgramInfoLog programId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  logStrLn $ "Program linking log:"
  logStrLn logMessage
  when (not success) exitFailure

  forM_ [("Vertex",vShader),("Fragment",fShader)] $ \(name,shader) -> do
    logStrLn $ "Detaching " ++ name ++ " Shader..."
    glDetachShader programId shader
    logStrLn $ "Deleting " ++ name ++ " Shader..."
    glDeleteShader shader

  [modelId,viewId,projectionId,lightId,textureSamplerId'] <- mapM (flip withCString (glGetUniformLocation programId)) ["Model","View","Projection","LightPosition","TextureSampler"]

  pure $ OpenGLInfo 
    {attributeBuffers =
      [VertexAttribute "position" vbo 0 3 GL_FLOAT GL_FALSE 0 nullPtr
      ,VertexAttribute "texture coordinate" tbo 1 2 GL_FLOAT GL_FALSE 0 nullPtr
      ,VertexAttribute "normal" nbo 2 3 GL_FLOAT GL_FALSE 0 nullPtr
      ]
    ,indexBuffer = ibo
    ,indexCount = fromIntegral $ length indexData * 3
    ,shaderProgram = programId
    ,modelMatrixId = modelId
    ,viewMatrixId = viewId
    ,projectionMatrixId = projectionId
    ,lightLocationId = lightId
    ,textureId = textureId'
    ,textureSamplerId = textureSamplerId'
    }

{-
stdOrtho :: Mat.Matrix Double
stdOrtho = Linear.V4
  [[1,0,0,0]
  ,[0,1,0,0]
  ,[0,0,-1,0]
  ,[0,0,0,1]
  ]
-}

-- TODO: natural inverse for faster yay
-- | Build a view matrix for looking in the direction of a unit vector from a point.
lookIn :: Num a => Linear.V3 a -> Linear.Quaternion a -> Linear.M44 a
lookIn eye forward = Linear.mkTransformation forward eye

-- | Pretty(ish) print a matrix-like structure
prettyMatrix :: (Foldable f, Foldable g, Show a) => f (g a) -> String
prettyMatrix = foldMap ((++"\n") . foldMap ((++" \t") . show))

-- | Load a shader of the specified type from the specified file
loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderType shaderPath = do
  logStrLn $ "Creating " ++ shaderPath ++ "..."
  shaderId <- glCreateShader shaderType
  logStrLn $ "Loading " ++ shaderPath ++ " source..."
  shaderSource <- readFile shaderPath 
  withCString shaderSource (\cs -> new cs >>= \s -> glShaderSource shaderId 1 s nullPtr)
  logStrLn $ "Compiling " ++ shaderPath ++ "..."
  glCompileShader shaderId
  logStr $ "Checking " ++ shaderPath ++ "..."
  (status,logLength) <- alloca $ \result -> do
    glGetShaderiv shaderId GL_COMPILE_STATUS result
    status <- peek result
    glGetShaderiv shaderId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)
  let success = fromIntegral status == GL_TRUE
  logStrLn $ "compile status was " ++ (if success then green else red) (show status)
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetShaderInfoLog shaderId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  logStrLn $ shaderPath ++ " compilation log:"
  logStrLn logMessage
  when (not success) exitFailure
  pure shaderId

-- | Draw a frame
draw :: OpenGLInfo -> (Linear.M44 GLfloat,Linear.M44 GLfloat,Linear.M44 GLfloat,Linear.V3 GLfloat) -> IO ()
draw OpenGLInfo{..} (m,v,p,l) = do
  logStrLn "Clearing screen..."
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  logStrLn "Using shader program..."
  glUseProgram shaderProgram
  forM_ [("Model",modelMatrixId,m),("View",viewMatrixId,v),("Projection",projectionMatrixId,p)] $ \(matName,matId,mat) -> do
    logStrLn $ "Sending " ++ matName ++ " matrix..."
    logStrLn $ prettyMatrix $ mat
    with mat $ glUniformMatrix4fv matId 1 GL_TRUE . (castPtr :: Ptr (Linear.M44 GLfloat) -> Ptr GLfloat)

  -- Send light position
  logStrLn "Sending light position..."
  with l $ glUniform3fv lightLocationId 1 . castPtr

  logStrLn "Binding texture to unit 0..."
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D textureId
  logStrLn "Unleashing texture sampler on unit 0..."
  glUniform1i textureSamplerId 0

  forM_ attributeBuffers $ \(VertexAttribute{..}) -> do
    logStrLn $ "Enabling vertex " ++ attributeName ++ " attribute..."
    glEnableVertexAttribArray attributeIndex
    logStrLn $ "Binding buffer for vertex " ++ attributeName ++ " attribute..."
    glBindBuffer GL_ARRAY_BUFFER attributeObjectName
    logStrLn $ "Defining vertex " ++ attributeName ++ " attribute attributes..."
    glVertexAttribPointer attributeIndex attributeSize attributeType attributeNormalized attributeStride attributeOffset

  logStrLn "Binding index buffer..."
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
  logStrLn "Drawing triangles..."
  glDrawElements GL_TRIANGLES indexCount GL_UNSIGNED_SHORT nullPtr

  forM_ attributeBuffers $ \(VertexAttribute{..}) -> do
    logStrLn $ "Disabling vertex " ++ attributeName ++ " attribute..."
    glDisableVertexAttribArray attributeIndex

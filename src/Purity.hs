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
import Foreign hiding (rotate)
import Foreign.C.String
import Control.Monad
import qualified Codec.Wavefront as Wavefront
import Data.Void
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TQueue as TQ
import System.IO.Unsafe (unsafePerformIO)
import Linear
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
  {_playerBody :: FreeBody V3 Float
  ,_playerForward :: Quaternion Float -- Rotate 0 0 -1 to the player's view
  ,_playerMotion :: Motion Float V3 Float
  ,_frameTime :: Float
  ,_lightPosition :: V3 Float
  }
makeLenses ''PurityState

instance Show PurityState where
  show s = 
    "{Player Body: " ++ s^.playerBody.to show ++
    " | Player Forward: " ++ s^.playerForward.to show ++
    " | Player Motion: " ++ s^.playerMotion.motionSpans.to show ++
    " | Player End Time Acceleration: " ++ s^.playerMotion.motionFinalAcceleration.to show ++
    " | Frame Time: " ++ s^.frameTime.to show ++
    " | Light Position: " ++ s^.lightPosition.to show ++ "}"

-- | Log a string asynchronously
logStr :: String -> IO ()
logStr = STM.atomically . TQ.writeTQueue globalLogger

-- | Log a string asynchronously and put a newline on it
logStrLn :: String -> IO ()
logStrLn = logStr . (++"\n")

-- | Log a string asyncronously with a tag and a newline
logStrTag :: String -> String -> IO ()
logStrTag tag str = logStrLn $ "[\x1b[34m" ++ tag ++ "\x1b[0m]" ++ str

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
  logTag . ("Using GLFW version " ++) . show =<< GLFW.getVersion
  logTag "Initializing GLFW... "
  GLFW.init >>= \case
    True -> do
      logTag $ green "Success!"
      logTag "Hinting GLFW: {Samples -> 4, OpenGL -> Forward Compatible 3.3 Core}"
      GLFW.windowHint $ GLFW.WindowHint'Samples 4
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      logTag "Creating fullscreen 1024x768 window... "
      let
        windowX = 1024
        windowY = 768
      GLFW.createWindow windowX windowY "Purity" Nothing Nothing >>= \case
        Nothing -> do
          logTag $ red "Failure!"
          GLFW.terminate
        Just theWindow -> do
          logTag $ green "Success!"
          logTag "Making the window the current context..."
          GLFW.makeContextCurrent (Just theWindow)
          logTag "Setting keyboard input mode so we can get events..."
          GLFW.setStickyKeysInputMode theWindow GLFW.StickyKeysInputMode'Enabled
          logTag "Setting mouse input mode so it doesn't move outside the window..."
          GLFW.setCursorInputMode theWindow GLFW.CursorInputMode'Disabled
          logTag "Polling events once to get things started..."
          GLFW.pollEvents
          logTag "Setting mouse to center screen..."
          GLFW.setCursorPos theWindow (fromIntegral windowX/2) (fromIntegral windowY/2)
          logTag "Initializing OpenGL..."
          openGLInfo <- initGL
          logTag "Entering render loop..."
          Just ft0 <- (fmap realToFrac) <$> GLFW.getTime
          let bodyPosition = V3 0 0 0
          renderLoop PurityState
            {_lightPosition = V3 4 3 3
            ,_playerBody = bodyAtRest bodyPosition
            ,_playerForward = normalize $ axisAngle (negate bodyPosition) 0
            ,_playerMotion = motionFromTimeline [spanDurationForce 0.001 (V3 1 0 0), spanDurationForce 0.001 (V3 (-1) 0 0)]
            ,_frameTime = ft0
            } openGLInfo theWindow
    False -> logTag "Failure!"
  where
    logTag = logStrTag "purityMain"

-- | A thread that logs things from the global logger forever
loggingThread :: IO Void
loggingThread = (STM.atomically (TQ.readTQueue globalLogger) >>= putStr) *> loggingThread

-- | A loop that renders the scene until the program ends
renderLoop :: PurityState -> OpenGLInfo -> GLFW.Window -> IO ()
renderLoop !state openGLInfo theWindow = do
  -- Compute matricies for this frame
  let
    mMat = V4
      (V4 0.1 0 0 0)
      (V4 0 0.1 0 0)
      (V4 0 0 0.1 0)
      (V4 0 0 0 1)
    vMat = inv44 $ lookIn (state^.playerBody.freeBodyPosition) (state^.playerForward)
    pMat = perspective (45 * pi/180) (4/3) 0.1 100

  -- Render this frame
  logTag "Drawing..."
  draw openGLInfo (mMat,vMat,pMat,state^.lightPosition)
  logTag "Swapping buffers..."
  GLFW.swapBuffers theWindow

  -- Prepare for next frame
  logTag "Polling events..."
  GLFW.pollEvents

  logTag  "Getting time..."
  Just ft <- (fmap realToFrac) <$> GLFW.getTime
  let
    dt = ft - state^.frameTime
    forward = rotate (state^.playerForward) $ V3 0 0 (-1)
    right = normalize $ cross forward (rotate (state^.playerForward) $ V3 0 1 0)
    up = normalize $ cross right forward

  -- Mouse look
  (cx,cy) <- GLFW.getCursorPos theWindow
  (wx,wy) <- GLFW.getWindowSize theWindow
  GLFW.setCursorPos theWindow (fromIntegral wx/2) (fromIntegral wy/2)
  let 
    (dx,dy) = ((fromIntegral wx/2)-cx,(fromIntegral wy/2)-cy)
    mouseSpeed = dt * 0.01 -- radians/pixel
    (tx,ty) = (realToFrac dx * mouseSpeed,realToFrac dy * mouseSpeed) :: (Float,Float)
    yaw = axisAngle {-(V3 0 1 0)-} up tx
    -- Clamp so they can't go upside down
    currty = acos $ dot (V3 0 (-1) 0) forward
    ty' = max (min ty (pi - currty)) (-currty)
    pitch = axisAngle right ty'

  logTag $ "Read cursor at " ++ show (cx,cy) ++ " and noted the change was " ++ show (dx,dy) ++ " off from " ++ show (fromIntegral wx/2 :: Float,fromIntegral wy/2 :: Float) ++ " so used angles " ++ show (tx,ty)

  -- Move player
  moveVec <- sum . zipWith (\v b -> if b then v else V3 0 0 0) [forward,-right,-forward,right,up,-up] . fmap (==GLFW.KeyState'Pressed) <$> mapM (GLFW.getKey theWindow) [GLFW.Key'W,GLFW.Key'A,GLFW.Key'S,GLFW.Key'D,GLFW.Key'Q,GLFW.Key'Z]

  -- Get light key
  lPressed <- (==GLFW.KeyState'Pressed) <$> GLFW.getKey theWindow GLFW.Key'L

  let 
    -- Update light position if need
    updateLight = if lPressed then lightPosition .~ (state^.playerBody.freeBodyPosition) else id

    -- Update with mouse look
    updateForward = playerForward %~ \q -> normalize (pitch * yaw * q)

    -- Apply forces to change velocity
    mu = 1
    friction = mu *^ negate (state^.playerBody.freeBodyVelocity)
    moveSpeed = 10
    moveForce = moveSpeed *^ moveVec
    netForce = friction ^+^ moveForce
    -- Uses incorrect frame time TODO
    updatePhysics = playerMotion %~ mixInForce 0 (spanDurationForce 0.01 netForce) . seekMotionForward 1

    -- Setup motion for next frame
    -- jk

    -- Set the player body to the updated position based on prexisting motion
    sampleFromMotion = playerBody %~ sampleMotion (state^.playerMotion) groundSurface dt
    state' = updateLight . updateForward . (frameTime .~ ft) . updatePhysics . sampleFromMotion $ state

  logTag $ "moveForce: " ++ show moveForce
  --logTag $ "New motion at current frame time: " ++ show (state'^.playerMotion.sampleMotion ft)
  logTag $ "New state: " ++ show state'

  GLFW.windowShouldClose theWindow >>= \case
    True -> do
      logTag "We've been told to close the window, doing so..."
      GLFW.destroyWindow theWindow
    False -> GLFW.getKey theWindow GLFW.Key'Escape >>= \case
      GLFW.KeyState'Pressed -> do
        logTag "Escape key was pressed, closing window"
        GLFW.destroyWindow theWindow
      _ -> Concurrent.threadDelay 1000 *> if not handBrake then renderLoop state' openGLInfo theWindow else logTag "Hard braking"
  where
    logTag = logStrTag "renderLoop"

handBrake :: Bool
handBrake = False

bufferGLData :: forall a. (Show a,Storable a) => String -> GLuint -> GLenum -> [a] -> IO ()
bufferGLData name bufferName bufferType bufferData = do
  logTag $ name ++ " data is:"
  mapM_ (logStrTag "Data Buffering") $ take 3 (map show bufferData) ++ ["..."] ++ drop (length bufferData - 3) (map show bufferData)
  logTag $ "Binding " ++ name ++ " buffer..."
  glBindBuffer bufferType bufferName
  logTag $ "Buffering " ++ name ++ " data..."
  withArray bufferData $ \bufferDataArray -> do
    glBufferData bufferType (fromIntegral $ length bufferData * sizeOf (undefined :: a)) bufferDataArray GL_STATIC_DRAW
  where
    logTag = logStrTag "bufferGLData"


-- | Initialize OpenGL, returning the rendering context
initGL :: IO OpenGLInfo
initGL = do
  logTag "Setting clear color to blue..."
  glClearColor 0 0 4 0

  logTag "Enabling depth testing..."
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS

  logTag "Enabling back face culling..."
  glEnable GL_CULL_FACE

  logTag "Generating Vertex Array Object Name..."
  vao <- alloca $ \namePtr -> do
    glGenVertexArrays 1 namePtr
    peek namePtr
  logTag $ "Vertex Array Object: " ++ show vao

  logTag "Generating Vertex Buffer Names..."
  [vbo,tbo,nbo,ibo] <- alloca $ \namePtr -> do
    glGenBuffers 4 namePtr
    peekArray 4 namePtr
  logTag $ "Vertex buffer: " ++ show vbo
  logTag $ "Texture coordinate buffer: " ++ show tbo
  logTag $ "Normal buffer: " ++ show nbo
  logTag $ "Index buffer: " ++ show ibo

  logTag "Generating texture name..."
  textureId' <- alloca $ \namePtr -> glGenTextures 1 namePtr *> peek namePtr
  logTag "Binding texture..."
  glBindTexture GL_TEXTURE_2D textureId'

  logTag "Loading texture juicily from file..."
  Right dynImage <- Juicy.readPng "test.png"
  let testTexture = Juicy.convertRGB8 dynImage

  logTag "Sending texture to GL..."
  Storable.unsafeWith (Juicy.imageData testTexture) $ glTexImage2D
    GL_TEXTURE_2D
    0 -- Mipmap level (0 full res)
    (fromIntegral GL_RGB) -- internal format
    (fromIntegral $ Juicy.imageWidth testTexture)
    (fromIntegral $ Juicy.imageHeight testTexture)
    0 -- This is 0 because OpenGL. L
    GL_RGB -- stored format
    GL_UNSIGNED_BYTE -- size of color component

  logTag "Configuring mipmaps..."
  logTag "  Magnify using linear filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
  logTag "  Minify using linear blending and filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
  logTag "Generating mipmaps..."
  glGenerateMipmap GL_TEXTURE_2D

  logTag "Binding VAO..."
  glBindVertexArray vao
  logTag "Buffering vertex attributes..."

  logTag "Loading model..."
  model <- Wavefront.fromFile "normalTeapot.obj" >>= \case
    Right m -> pure m
    Left e -> error e

  let vertexData = fmap (\(Wavefront.Location x y z _w) -> V3 x y z) . Vector.toList $ Wavefront.objLocations model
  bufferGLData "position" vbo GL_ARRAY_BUFFER (vertexData :: [V3 GLfloat])

  let textureData = fmap (\(Wavefront.TexCoord r s _t) -> V2 r s) . Vector.toList . Wavefront.objTexCoords $ model
  bufferGLData "texture coordinate" tbo GL_ARRAY_BUFFER (textureData :: [V2 GLfloat])

  let normalData = fmap (\(Wavefront.Normal x y z) -> V3 x y z) . Vector.toList . Wavefront.objNormals $ model
  bufferGLData "normal" nbo GL_ARRAY_BUFFER (normalData :: [V3 GLfloat])

  let indexData = fmap (fmap (fromIntegral . subtract 1 . Wavefront.faceLocIndex) . (\(Wavefront.Face a b c _fs) -> V3 a b c) . Wavefront.elValue) . Vector.toList . Wavefront.objFaces $ model
  bufferGLData "index" ibo GL_ELEMENT_ARRAY_BUFFER (indexData :: [V3 GLushort])

  logTag "Loading Vertex Shader..."
  vShader <- loadShader GL_VERTEX_SHADER "vertexShader"
  logTag "Loading Fragment Shader..."
  fShader <- loadShader GL_FRAGMENT_SHADER "fragmentShader"

  logTag "Creating program..."
  programId <- glCreateProgram

  forM_ [("Vertex",vShader),("Fragment",fShader)] $ \(name,shader) -> do
    logTag $ "Attaching " ++ name ++ " Shader..."
    glAttachShader programId shader

  logTag "Linking program..."
  glLinkProgram programId
  logTag "Checking program..."
  (status,logLength) <- alloca $ \result -> do
    glGetProgramiv programId GL_LINK_STATUS result
    status <- peek result
    glGetProgramiv programId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)
  let success = fromIntegral status == GL_TRUE
  logTag $ "link status was " ++ (if success then green else red) (show status)
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetProgramInfoLog programId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  logTag $ "Program linking log:"
  logTag logMessage
  when (not success) exitFailure

  forM_ [("Vertex",vShader),("Fragment",fShader)] $ \(name,shader) -> do
    logTag $ "Detaching " ++ name ++ " Shader..."
    glDetachShader programId shader
    logTag $ "Deleting " ++ name ++ " Shader..."
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
  where
    logTag = logStrTag "initGL"

{-
stdOrtho :: Mat.Matrix Double
stdOrtho = V4
  [[1,0,0,0]
  ,[0,1,0,0]
  ,[0,0,-1,0]
  ,[0,0,0,1]
  ]
-}

-- TODO: natural inverse for faster yay
-- | Build a view matrix for looking in the direction of a unit vector from a point.
lookIn :: (Conjugate a,RealFloat a) => V3 a -> Quaternion a -> M44 a
lookIn eye forward = mkTransformation forward eye

-- | Pretty(ish) print a matrix-like structure
prettyMatrix :: (Foldable f, Foldable g, Show a) => f (g a) -> String
prettyMatrix = foldMap ((++"\n") . foldMap ((++" \t") . show))

-- | Load a shader of the specified type from the specified file
loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderType shaderPath = do
  logTag $ "Creating " ++ shaderPath ++ "..."
  shaderId <- glCreateShader shaderType
  logTag $ "Loading " ++ shaderPath ++ " source..."
  shaderSource <- readFile shaderPath 
  withCString shaderSource (\cs -> new cs >>= \s -> glShaderSource shaderId 1 s nullPtr)
  logTag $ "Compiling " ++ shaderPath ++ "..."
  glCompileShader shaderId
  logTag $ "Checking " ++ shaderPath ++ "..."
  (status,logLength) <- alloca $ \result -> do
    glGetShaderiv shaderId GL_COMPILE_STATUS result
    status <- peek result
    glGetShaderiv shaderId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)
  let success = fromIntegral status == GL_TRUE
  logTag $ "compile status was " ++ (if success then green else red) (show status)
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetShaderInfoLog shaderId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  logTag $ shaderPath ++ " compilation log:"
  logTag logMessage
  when (not success) exitFailure
  pure shaderId
  where
    logTag = logStrTag "loadShader"

-- | Draw a frame
draw :: OpenGLInfo -> (M44 GLfloat,M44 GLfloat,M44 GLfloat,V3 GLfloat) -> IO ()
draw OpenGLInfo{..} (m,v,p,l) = do
  logTag "Clearing screen..."
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  logTag "Using shader program..."
  glUseProgram shaderProgram
  forM_ [("Model",modelMatrixId,m),("View",viewMatrixId,v),("Projection",projectionMatrixId,p)] $ \(matName,matId,mat) -> do
    logTag $ "Sending " ++ matName ++ " matrix..."
    logTag $ prettyMatrix $ mat
    with mat $ glUniformMatrix4fv matId 1 GL_TRUE . (castPtr :: Ptr (M44 GLfloat) -> Ptr GLfloat)

  -- Send light position
  logTag "Sending light position..."
  with l $ glUniform3fv lightLocationId 1 . castPtr

  logTag "Binding texture to unit 0..."
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D textureId
  logTag "Unleashing texture sampler on unit 0..."
  glUniform1i textureSamplerId 0

  forM_ attributeBuffers $ \(VertexAttribute{..}) -> do
    logTag $ "Enabling vertex " ++ attributeName ++ " attribute..."
    glEnableVertexAttribArray attributeIndex
    logTag $ "Binding buffer for vertex " ++ attributeName ++ " attribute..."
    glBindBuffer GL_ARRAY_BUFFER attributeObjectName
    logTag $ "Defining vertex " ++ attributeName ++ " attribute attributes..."
    glVertexAttribPointer attributeIndex attributeSize attributeType attributeNormalized attributeStride attributeOffset

  logTag "Binding index buffer..."
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
  logTag "Drawing triangles..."
  glDrawElements GL_TRIANGLES indexCount GL_UNSIGNED_SHORT nullPtr

  forM_ attributeBuffers $ \(VertexAttribute{..}) -> do
    logTag $ "Disabling vertex " ++ attributeName ++ " attribute..."
    glDisableVertexAttribArray attributeIndex
  where
    logTag = logStrTag "draw"

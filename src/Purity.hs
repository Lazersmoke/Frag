{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Yay
module Purity where

import Graphics.GL.Core45
import qualified Graphics.UI.GLFW as GLFW
import Foreign hiding (rotate)
import Foreign.C.String
import Control.Monad
import qualified Control.Concurrent as Concurrent
import Linear
import Control.Lens
import System.Exit

import Purity.Physics
import Purity.Render
import Purity.Data

data Presence q a = Presence
  {visiblePresence :: DrawModel
  ,physicalPresence :: PhysModel q a
  }

-- | A type encapsulating all state in purity
data PurityState = PurityState
  {_cameraPosition :: V3 Float
  ,_cameraForward :: Quaternion Float -- Rotate 0 0 -1 to the camera's view
  ,_frameTimeInitial :: Float
  ,_frameTime :: Float
  ,_lightPosition :: V3 Float
  ,_presences :: [Presence V3 Float]
  }
makeLenses ''PurityState

defaultPurityState :: PurityState
defaultPurityState = PurityState
  {_cameraPosition = V3 0 0 50
  ,_cameraForward = axisAngle (V3 0 0 (-1)) 0
  ,_frameTimeInitial = 0
  ,_frameTime = 0
  ,_lightPosition = V3 4 3 3
  ,_presences = []
  }

instance Show PurityState where
  show s = 
    "{Camera Position: " ++ s^.cameraPosition.to show ++
    " | Camera Forward: " ++ s^.cameraForward.to show ++
    " | Frame Time Initial: " ++ s^.frameTimeInitial.to show ++
    " | Frame Time: " ++ s^.frameTime.to show ++
    " | Light Position: " ++ s^.lightPosition.to show ++ "}"

aabbContaining :: RenderModel -> PhysDomain V3 GLfloat
aabbContaining rm = AABB (minimum <$> trans) (maximum <$> trans)
  where
    trans = sequenceA (modelVerticies rm)

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
          --GLFW.setCursorInputMode theWindow GLFW.CursorInputMode'Disabled
          logTag "Polling events once to get things started..."
          GLFW.pollEvents
          logTag "Setting mouse to center screen..."
          --GLFW.setCursorPos theWindow (fromIntegral windowX/2) (fromIntegral windowY/2)
          logTag "Initializing OpenGL..."
          openGLInfo <- initGL
          logTag "Adding Teapot..."
          rmTeapot <- initModel "normalTeapot.obj"
          let 
           teapot = Presence
            {visiblePresence = loadedModel rmTeapot
            ,physicalPresence = PhysModel
              {_physDomain = aabbContaining rmTeapot
              ,_currentVelocity = V3 1 5 0
              ,_currentOrigin = V3 0 0 0
              ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
              }
            }
          logTag "Entering render loop..."
          Just ft0 <- (fmap realToFrac) <$> GLFW.getTime
          renderLoop ((presences .~ [teapot]) . (frameTimeInitial .~ ft0) . (frameTime .~ ft0) $ defaultPurityState) openGLInfo theWindow
    False -> logTag "Failure!"
  where
    logTag = logStrTag "purityMain"


-- | A loop that renders the scene until the program ends
renderLoop :: PurityState -> OpenGLInfo -> GLFW.Window -> IO ()
renderLoop !state openGLInfo theWindow = do
  -- Compute updated physics models for this frame
  renderables <- forM (state^.presences) $ \pres -> do
    let tea' = sampleModelForward (state^.frameTime - state^.frameTimeInitial) (V3 0 (-5) 0) (physicalPresence pres)
    pure RenderSpec
      {modelMatrix = mkTransformation (tea'^.currentOrientation) (tea'^.currentOrigin)
      ,viewMatrix = inv44 $ lookIn (state^.cameraPosition) (state^.cameraForward)
      ,projMatrix = perspective (45 * pi/180) (4/3) 0.1 100
      ,lightPos = state^.lightPosition
      ,modelToRender = visiblePresence pres
      }

  -- Render this frame
  logTag "Drawing..."
  draw openGLInfo renderables
  logTag "Swapping buffers..."
  GLFW.swapBuffers theWindow

  -- Prepare for next frame
  logTag "Polling events..."
  GLFW.pollEvents

  logTag  "Getting time..."
  Just ft <- (fmap realToFrac) <$> GLFW.getTime
  let
    dt = ft - state^.frameTime
    forward = rotate (state^.cameraForward) $ V3 0 0 (-1)
    right = normalize $ cross forward (rotate (state^.cameraForward) $ V3 0 1 0)
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
    updateLight = if lPressed then lightPosition .~ (state^.cameraPosition) else id
    -- Update with mouse look
    updateForward = cameraForward %~ \q -> q + 0 *^ normalize (pitch * yaw * q)
    -- Update with keyboard
    updateMove = cameraPosition %~ (^+^ 0 *^ moveVec)
    -- Apply all updates
    state' = updateLight . updateForward . updateMove . (frameTime .~ ft) $ state

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
    {shaderProgram = programId
    ,modelMatrixId = modelId
    ,viewMatrixId = viewId
    ,projectionMatrixId = projectionId
    ,lightLocationId = lightId
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
draw :: OpenGLInfo -> [RenderSpec] -> IO ()
draw glInfo@OpenGLInfo{..} models = do
  logTag "Clearing screen..."
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  logTag "Using shader program..."
  glUseProgram shaderProgram
  logTag "Rendering all models..."
  forM_ models (drawModel glInfo)
  {-forM_ attributeBuffers $ \(VertexAttribute{..}) -> do
    logTag $ "Disabling vertex " ++ attributeName ++ " attribute..."
    glDisableVertexAttribArray attributeIndex
    -}
  where
    logTag = logStrTag "draw"

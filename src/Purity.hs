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
  {_visiblePresence :: DrawModel
  ,_physicalPresence :: PhysStory q a
  }
makeLenses ''Presence

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
  {_cameraPosition = V3 0 0 10
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

aabbContaining :: ModelData -> PhysDomain V3 GLfloat
aabbContaining md = AABBDomain ((maximum <$> trans) ^-^ (minimum <$> trans))
  where
    trans = sequenceA (modelVerticies md)

wireframeAABB :: PhysDomain V3 GLfloat -> IO DrawModel
wireframeAABB (AABBDomain (V3 x y z)) = initLines vs is
  where
    vs =
      [V3 0 0 0,V3 0 0 z,V3 0 y 0,V3 0 y z
      ,V3 x 0 0,V3 x 0 z,V3 x y 0,V3 x y z
      ]
    is =
      [0,4,1,5,2,6,3,7
      ,0,2,1,3,4,6,5,7
      ,0,1,2,3,4,5,6,7]

-- | The main entry point
purityMain :: IO ()
purityMain = do
  _ <- Concurrent.forkIO (() <$ loggingThread)
  logInfo . ("Using GLFW version " ++) . show =<< GLFW.getVersion
  logInfo "Initializing GLFW... "
  GLFW.init >>= \case
    True -> do
      logInfo $ green "Success!"
      logInfo "Hinting GLFW: {Samples -> 4, OpenGL -> Forward Compatible 3.3 Core}"
      GLFW.windowHint $ GLFW.WindowHint'Samples 4
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      logInfo "Creating 1024x768 window... "
      let
        windowX = 1024
        windowY = 768
      GLFW.createWindow windowX windowY "Purity" Nothing Nothing >>= \case
        Nothing -> do
          logInfo $ red "Failure!"
          GLFW.terminate
        Just theWindow -> do
          logInfo $ green "Success!"
          logInfo "Making the window the current context..."
          GLFW.makeContextCurrent (Just theWindow)
          logInfo "Setting keyboard input mode so we can get events..."
          GLFW.setStickyKeysInputMode theWindow GLFW.StickyKeysInputMode'Enabled
          logInfo "Setting mouse input mode so it doesn't move outside the window..."
          --GLFW.setCursorInputMode theWindow GLFW.CursorInputMode'Disabled
          logInfo "Polling events once to get things started..."
          GLFW.pollEvents
          logInfo "Setting mouse to center screen..."
          --GLFW.setCursorPos theWindow (fromIntegral windowX/2) (fromIntegral windowY/2)
          logInfo "Initializing OpenGL..."
          initGL
          textShader <- createShaderProgram "textVShader" "textFShader" ["projection","text","textColor"]
          flatModelShader <- createShaderProgram "flatVertexShader" "flatFragmentShader" ["Model","View","Projection","LightPosition","FlatColor"]
          texturedModelShader <- createShaderProgram "vertexShader" "fragmentShader" ["Model","View","Projection","LightPosition","TextureSampler"]

          --(rmTeapot, teapotData) <- initModel "normalTeapot.obj" "test.png"
          (rmArrow, _arrowData) <- initFlatModel "unitzarrow.obj"
          let fallingAABB = AABBDomain (V3 1 1 1)
          wirePot <- wireframeAABB fallingAABB
          wireGround <- wireframeAABB $ objAtZero^.presentModel.physDomain
          let 
            teapotOne = Presence
              {_visiblePresence = wirePot
              ,_physicalPresence = singleChapter . gravityFuture . domainAtRest (V3 1 5 0) $ fallingAABB
              }
            teapotTwo = physicalPresence %~ (spliceBump 0.5 (V3 0 3 0)) $ teapotOne
            groundCube = Presence
              {_visiblePresence = wireGround
              ,_physicalPresence = singleChapter objAtZero
              }
            considerGround = physicalPresence %~ collideWith objAtZero
          logInfo "Entering render loop..."
          Just ft0 <- (fmap realToFrac) <$> GLFW.getTime
          renderLoop ((presences .~ [groundCube, considerGround teapotOne,considerGround teapotTwo]) . (frameTimeInitial .~ ft0) . (frameTime .~ ft0) $ defaultPurityState) rmArrow flatModelShader texturedModelShader textShader theWindow
    False -> logInfo "Failure!"

-- | A loop that renders the scene until the program ends
renderLoop :: PurityState -> DrawModel -> ShaderProgram -> ShaderProgram -> ShaderProgram -> GLFW.Window -> IO ()
renderLoop !state arrowDrawModel flatModelShader texturedModelShader textShader theWindow = do
  -- Compute updated physics models for this frame
  let 
    frameSpeed = 0.25
    scaledt = frameSpeed * (state^.frameTime - state^.frameTimeInitial)
    onThisFrame pres = readStory scaledt $ pres^.physicalPresence
    renderedPhysModels = map onThisFrame (state^.presences)
    texturedModels = map texturedModel (state^.presences)
    texturedModel pres = RenderSpec
      {modelMatrix = mkTransformation (onThisFrame pres^.currentOrientation) (onThisFrame pres^.currentOrigin)
      ,modelToRender = DrawIndexedModel $ pres^.visiblePresence
      ,renderItemName = "PhysModel with " ++ show (onThisFrame pres)
      }
    cameraSpinSpeed = 0.1
    viewMatrix = lookAt (20 *^ (V3 (cos $ cameraSpinSpeed * scaledt) 0 (sin $ cameraSpinSpeed * scaledt))) (V3 0 0 0) (V3 0 1 0) --inv44 $ lookIn (state^.cameraPosition) (state^.cameraForward)
    projMatrix = perspective (45 * pi/180) (4/3 :: GLfloat) 0.1 100
    lightPos = state^.lightPosition
    arrowModelDirection = V3 0 1 0
    axisArrows = [arrowFromTo zero (V3 0 0 1),arrowFromTo zero (V3 0 1 0),arrowFromTo zero (V3 1 0 0)]
    velocityArrows = map velocityArrow renderedPhysModels
    velocityArrow m = arrowFromTo (m^.currentOrigin) ((m^.currentOrigin) ^+^ (m^.currentVelocity))
    accelArrows = map accelArrow (state^.presences)
    accelArrow p = arrowFromTo (onThisFrame p^.currentOrigin) ((onThisFrame p^.currentOrigin) ^+^ (p^.physicalPresence.initialModel.appliedForce))
    arrowFromTo f t = RenderSpec
      {modelMatrix = mkTransformationMat (fromQuaternion $ deltaQuat arrowModelDirection (normalize $ t - f)) f !*! (V4 (V4 1 0 0 0) (V4 0 (norm $ t - f) 0 0) (V4 0 0 1 0) (V4 0 0 0 1))
      ,modelToRender = DrawIndexedModel arrowDrawModel
      ,renderItemName = "Arrow from " ++ show f ++ " to " ++ show t
      }
    deltaQuat :: V3 GLfloat -> V3 GLfloat -> Quaternion GLfloat
    deltaQuat a b = if nearZero $ a ^-^ b
      then 1
      else if nearZero $ a ^+^ b
        then if nearZero $ cross a (V3 1 0 0)
          then axisAngle (cross a (V3 0 1 0)) pi
          else axisAngle (cross a (V3 1 0 0)) pi
        else axisAngle (cross a b) (acos $ dot a b)
  drStr <- renderString (25,25) (show $ state^.frameTime) >>= \di -> pure RenderSpec
    {modelMatrix = ortho 0 1024 0 768 (-1) 1
    ,modelToRender = di
    ,renderItemName = "debug text"
    }

  -- Render this frame
  logTick "Drawing..."
  logTick "Clearing screen..."
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  do
    logTick "Using textured model shader program..."
    glUseProgram (shaderName texturedModelShader)

    let [modelId,viewId,projectionId,lightLocationId,textureSamplerId] = shaderUniformNames texturedModelShader
    forM_ [("View",viewId,viewMatrix),("Projection",projectionId,projMatrix)] $ uncurry3 sendMatrix

    logTick "Sending light position..."
    with lightPos $ glUniform3fv lightLocationId 1 . castPtr

    logTick "Unleashing texture sampler on unit 0..."
    glUniform1i textureSamplerId 0

    logTick "Rendering all models..."
    forM_ texturedModels (drawModel modelId)

  do
    logTick "Using flat model shader program..."
    glUseProgram (shaderName flatModelShader)

    let [modelId,viewId,projectionId,lightLocationId,flatColor] = shaderUniformNames flatModelShader
    forM_ [("View",viewId,viewMatrix),("Projection",projectionId,projMatrix)] $ uncurry3 sendMatrix

    logTick "Sending light position..."
    with lightPos $ glUniform3fv lightLocationId 1 . castPtr

    logTick "Sending axis arrow color..."
    with (V3 1 0 1 :: V3 Float) $ glUniform3fv flatColor 1 . castPtr

    logTick "Rendering axis arrows..."
    forM_ axisArrows (drawModel modelId)

    logTick "Sending velocity arrow color..."
    with (V3 1 0 0 :: V3 Float) $ glUniform3fv flatColor 1 . castPtr

    logTick "Rendering all velocity arrows..."
    forM_ velocityArrows (drawModel modelId)

    logTick "Sending acceleration arrow color..."
    with (V3 0 1 0 :: V3 Float) $ glUniform3fv flatColor 1 . castPtr

    logTick "Rendering all acceleration arrows..."
    forM_ accelArrows (drawModel modelId)

  do
    logTick "Using text shader program..."
    glUseProgram (shaderName textShader)

    let [mvp,textSampler,textColor] = shaderUniformNames textShader

    logTick "Unleashing texture sampler on unit 0..."
    glUniform1i textSampler 0

    logTick "Sending text color..."
    with (V3 1 0 0 :: V3 Float) $ glUniform3fv textColor 1 . castPtr

    drawModel mvp drStr

  logTick "Swapping buffers..."
  GLFW.swapBuffers theWindow

  -- Prepare for next frame
  logTick "Polling events..."
  GLFW.pollEvents

  logTick  "Getting time..."
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

  logTick $ "Read cursor at " ++ show (cx,cy) ++ " and noted the change was " ++ show (dx,dy) ++ " off from " ++ show (fromIntegral wx/2 :: Float,fromIntegral wy/2 :: Float) ++ " so used angles " ++ show (tx,ty)

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

  logTick $ "New state: " ++ show state'

  GLFW.windowShouldClose theWindow >>= \case
    True -> do
      logInfo "We've been told to close the window, doing so..."
      GLFW.destroyWindow theWindow
    False -> GLFW.getKey theWindow GLFW.Key'Escape >>= \case
      GLFW.KeyState'Pressed -> do
        logInfo "Escape key was pressed, closing window"
        GLFW.destroyWindow theWindow
      _ -> Concurrent.threadDelay 1000 *> if not handBrake then renderLoop state' arrowDrawModel flatModelShader texturedModelShader textShader theWindow else logInfo "Hard braking"

handBrake :: Bool
handBrake = False

-- | Initialize OpenGL
initGL :: IO ()
initGL = do
  logInfo "Setting clear color to blue..."
  glClearColor 0 0 4 0

  logInfo "Enabling depth testing..."
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS

  logInfo "Enabling back face culling..."
  glEnable GL_CULL_FACE

createShaderProgram :: FilePath -> FilePath -> [String] -> IO ShaderProgram
createShaderProgram vShaderPath fShaderPath uniformNames = do
  programId <- initShaderProgram vShaderPath fShaderPath
  uniforms <- mapM (flip withCString (glGetUniformLocation programId)) uniformNames
  pure $ ShaderProgram
    {shaderName = programId
    ,shaderUniformNames = uniforms
    }
  
initShaderProgram :: FilePath -> FilePath -> IO GLuint
initShaderProgram vShaderPath fShaderPath = do
  logInfo "Loading Vertex Shader..."
  vShader <- loadShader GL_VERTEX_SHADER vShaderPath
  logInfo "Loading Fragment Shader..."
  fShader <- loadShader GL_FRAGMENT_SHADER fShaderPath

  logInfo "Creating shader program..."
  programId <- glCreateProgram

  forM_ [("Vertex",vShader),("Fragment",fShader)] $ \(name,shader) -> do
    logInfo $ "Attaching " ++ name ++ " Shader..."
    glAttachShader programId shader

  logInfo "Linking shader program..."
  glLinkProgram programId
  logInfo "Checking shader program..."
  (status,logLength) <- alloca $ \result -> do
    glGetProgramiv programId GL_LINK_STATUS result
    status <- peek result
    glGetProgramiv programId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)
  let success = fromIntegral status == GL_TRUE
  logInfo $ "link status was " ++ (if success then green else red) (show status)
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetProgramInfoLog programId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  logInfo $ "Program linking log:"
  logInfo logMessage
  when (not success) exitFailure

  forM_ [("Vertex",vShader),("Fragment",fShader)] $ \(name,shader) -> do
    logInfo $ "Detaching " ++ name ++ " Shader..."
    glDetachShader programId shader
    logInfo $ "Deleting " ++ name ++ " Shader..."
    glDeleteShader shader
  pure programId

stdOrtho :: M44 GLfloat
stdOrtho = V4
  (V4 1 0 0 0)
  (V4 0 1 0 0)
  (V4 0 0 (-1) 0)
  (V4 0 0 0 1)

-- TODO: natural inverse for faster yay
-- | Build a view matrix for looking in the direction of a unit vector from a point.
lookIn :: (Conjugate a,RealFloat a) => V3 a -> Quaternion a -> M44 a
lookIn eye forward = mkTransformation forward eye

-- | Load a shader of the specified type from the specified file
loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderType shaderPath = do
  logInfo $ "Creating " ++ shaderPath ++ "..."
  shaderId <- glCreateShader shaderType
  logInfo $ "Loading " ++ shaderPath ++ " source..."
  shaderSource <- readFile shaderPath 
  withCString shaderSource (\cs -> new cs >>= \s -> glShaderSource shaderId 1 s nullPtr)
  logInfo $ "Compiling " ++ shaderPath ++ "..."
  glCompileShader shaderId
  logInfo $ "Checking " ++ shaderPath ++ "..."
  (status,logLength) <- alloca $ \result -> do
    glGetShaderiv shaderId GL_COMPILE_STATUS result
    status <- peek result
    glGetShaderiv shaderId GL_INFO_LOG_LENGTH result
    logLength <- peek result
    pure (status,logLength)
  let success = fromIntegral status == GL_TRUE
  logInfo $ "compile status was " ++ (if success then green else red) (show status)
  logMessage <- allocaArray (fromIntegral logLength + 1) $ \msg -> glGetShaderInfoLog shaderId (fromIntegral logLength) nullPtr msg >> peekCStringLen (msg,fromIntegral logLength)
  logInfo $ shaderPath ++ " compilation log:"
  logInfo logMessage
  when (not success) exitFailure
  pure shaderId

-- | Draw a frame
{-
draw :: OpenGLInfo -> RenderContext -> [RenderSpec] -> IO ()
draw glInfo@OpenGLInfo{..} rCtx@RenderContext{..} models = do
  logTick "Clearing screen..."
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  logTick "Using shader program..."
  glUseProgram shaderProgram

  forM_ [("View",viewMatrixId,viewMatrix),("Projection",projectionMatrixId,projMatrix)] $ uncurry3 sendMatrix

  logTick "Sending light position..."
  with lightPos $ glUniform3fv lightLocationId 1 . castPtr

  logTick "Unleashing texture sampler on unit 0..."
  glUniform1i textureSamplerId 0

  logTick "Rendering all models..."
  forM_ models (drawModel glInfo rCtx)
  -}

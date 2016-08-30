{-# LANGUAGE LambdaCase #-}
module Purity.Client.Render where

import Purity.Client.Data
import Purity.Client.Util 

import Data.Access

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent
import Control.Monad
import Data.List


-- Intended to be forked from main thread almost right away
beginRenderLoop :: MVar Mode -> IO ()
beginRenderLoop mvarRender = do
  plog Log "Starting Render Loop"
  Just win <- initialize
  GLFW.makeContextCurrent (Just win)
  --GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
  GLFW.setCursorPos win 0 0
  GLFW.setWindowCloseCallback win (Just GLFW.destroyWindow)--I know how much you like explosions, Jimmy!
  GLFW.setKeyCallback win (Just $ keyboardCallback mvarRender) -- I pressa da button yass vary mucuh
  GLFW.setCursorPosCallback win (Just $ cursorPosCallback mvarRender) -- I pressa da button yass vary mucuh

  glProgram <- initOpenGL

  plog Log "OpenGL Window Created"
  renderLoop (renderCallback mvarRender) glProgram win
  GLFW.destroyWindow win
  GLFW.terminate

renderLoop :: Renderer -> Renderer
renderLoop drawFunc program win = do
  close <- GLFW.windowShouldClose win 
  unless close $ do
    drawFunc program win
    GLFW.swapBuffers win
    GLFW.pollEvents
    renderLoop drawFunc program win

      
initialize :: IO (Maybe GLFW.Window)
initialize = do
  plog Log "Initializing GLFW"
  -- TODO: Handle the output instead of black holing it
  _ <- GLFW.init
  GLFW.getVersion >>= print
  GLFW.setErrorCallback (Just breakTheGlass) -- Jimmy! Just do it!
  GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
  GLFW.createWindow
    640 -- width
    480 -- height
    "Purity Client" -- Title
    Nothing -- Monitor (no monitor yet)
    Nothing -- Window (no monitor yet)

-- Setup OpenGL and create and acitvate program
initOpenGL :: IO GL.Program
initOpenGL = do
  plog Log "Initializing OpenGL"

  program <- GL.createProgram

  -- Don't keep reference to shaders; they are already linked
  _ <- initShaders program
    [("Vertex Shader", GL.VertexShader, vsSource)
    ,("Fragment Shader", GL.FragmentShader, fsSource)
    ]

  -- setup program attributes
  GL.attribLocation program "coord" $= GL.AttribLocation 0
  GL.attribLocation program "color" $= GL.AttribLocation 1

  -- link program
  plog Log "Linking Shader Program"
  GL.linkProgram program
  programOk <- GL.get $ GL.linkStatus program

  -- validate it
  GL.validateProgram program
  programValid <- GL.get $ GL.validateStatus program

  -- show an error if something went wrong
  unless (programValid && programOk) $ do
    debugInfo <- GL.get (GL.programInfoLog program) 
    plog Error $ "OpenGL Program " ++ (if programOk then "Validation" else "Linking") ++ " Failed: " ++ debugInfo

  plog Log "Binding Shader Program"
  GL.currentProgram $= Just program

  GL.get GL.errors >>= mapM_ (plog Error . show)
  return program

initShaders :: GL.Program -> [(String, GL.ShaderType, String)] -> IO [Maybe GL.Shader]
initShaders program shades = forM shades $ \(shaderName, shaderType, source) -> do
  plog Log $ "Initializing " ++ shaderName
  theShader <- GL.createShader shaderType
  GL.shaderSourceBS theShader $= GL.packUtf8 source
  plog Log $ "Compiling " ++ shaderName
  GL.compileShader theShader
  GL.get (GL.compileStatus theShader) >>= \case
    False -> do
      err <- GL.get (GL.shaderInfoLog theShader)
      plog Error $ "Error compiling " ++ shaderName ++ ": " ++ err
      plog Error $ shaderName ++ " will not be linked to the program"
      return Nothing
    True -> do
      plog Log $ "Linking " ++ shaderName ++ " to program"
      GL.attachShader program theShader
      return $ Just theShader

breakTheGlass :: GLFW.ErrorCallback
breakTheGlass err desc = plog Error $ show err ++ " | " ++ desc

keyboardCallback :: MVar Mode -> GLFW.KeyCallback
keyboardCallback mvarRender w k i ks m = grab KeyCallback <$> readMVar mvarRender >>= \mode -> mode w k i ks m

cursorPosCallback :: MVar Mode -> GLFW.CursorPosCallback
cursorPosCallback mvarRender w x y = grab CursorPosCallback <$> readMVar mvarRender >>= \mode -> mode w x y

renderCallback :: MVar Mode -> Renderer
renderCallback mvarRender program win = grab DrawFunction <$> readMVar mvarRender >>= \mode -> mode program win

vsSource :: String
vsSource = intercalate "\n"
  ["attribute vec3 coord;"
  ,"attribute vec3 color;"
  ,"varying vec3 f_color;"
  ,"uniform mat4 projection;"
  ,"uniform mat4 look;"
  ,""
  ,"void main(){"
  ,"  gl_Position = (projection * look) * vec4(coord,1.0);"
  ,"  f_color = color;"
  ,"}"
  ]

fsSource :: String
fsSource = intercalate "\n"
  ["varying vec3 f_color;"
  ,""
  ,"void main(){"
  ,"  gl_FragColor = vec4(f_color, 1);"
  ,"}"
  ]

module Purity.Client.DefaultMode where

import Purity.Client.Data
import Purity.Client.Util

import qualified Graphics.UI.GLFW as GLFW

defaultKeyboardCallback :: GLFW.KeyCallback
defaultKeyboardCallback _ _ _ _ _ = plog Warn "Default Keyboard Callback Invoked!"

defaultMouseButtonCallback :: GLFW.MouseButtonCallback
defaultMouseButtonCallback _ _ _ _ = plog Warn "Default Mouse Button Callback Invoked!"

defaultCursorPosCallback :: GLFW.CursorPosCallback
defaultCursorPosCallback _ _ _ = plog Warn "Default Mouse Cursor Callback Invoked!"

defaultRenderCallback :: GLFW.Window -> IO ()
defaultRenderCallback _ = plog Warn "Default Render Callback Invoked!"

defaultMode :: Mode
defaultMode = buildMode defaultRenderCallback defaultKeyboardCallback defaultMouseButtonCallback defaultCursorPosCallback

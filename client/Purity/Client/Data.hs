{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Purity.Client.Data where

import Data.Access

import Control.Concurrent
import qualified Graphics.UI.GLFW as GLFW

data ServerMessage = NameQuery | LobbyUpdate [LobbyEntry] deriving Show

-- LobbyEntry Name Identifier ReadyStatus
data LobbyEntry = LobbyEntry {
  _name :: String, 
  _ident :: String, 
  _ready :: Bool
  } deriving Show

mkLobbyEntry :: String -> String -> Bool -> LobbyEntry
mkLobbyEntry name ident ready = LobbyEntry {_name = name, _ident = ident, _ready = ready}

data Name = Name
instance Access LobbyEntry String Name where
  grab _ = _name
  lift _ f e = e {_name = f $ _name e}

data Identity = Identity
instance Access LobbyEntry String Identity where
  grab _ = _ident
  lift _ f e = e {_ident = f $ _ident e}

data Ready = Ready
instance Access LobbyEntry Bool Ready where
  grab _ = _ready
  lift _ f e = e {_ready = f $ _ready e}


data Mode = Mode {
  _drawFunction :: GLFW.Window -> IO (),
  _keyboardCallback :: GLFW.KeyCallback,
  _mouseCallback :: GLFW.MouseButtonCallback
  }

buildMode :: (GLFW.Window -> IO ()) -> GLFW.KeyCallback -> GLFW.MouseButtonCallback -> Mode
buildMode draw key mouse = Mode {
  _drawFunction = draw,
  _keyboardCallback = key,
  _mouseCallback = mouse
  }


data DrawFunction = DrawFunction
instance Access Mode (GLFW.Window -> IO ()) DrawFunction where
  grab _ = _drawFunction
  lift _ f m = m {_drawFunction = f $ _drawFunction m}   

data KeyCallback = KeyCallback
instance Access Mode GLFW.KeyCallback KeyCallback where
  grab _ = _keyboardCallback
  lift _ f m = m {_keyboardCallback = f $ _keyboardCallback m}   

data MouseButtonCallback = MouseButtonCallback
instance Access Mode GLFW.MouseButtonCallback MouseButtonCallback where
  grab _ = _mouseCallback
  lift _ f m = m {_mouseCallback = f $ _mouseCallback m} 

data LogLevel = Log | Warn | Error

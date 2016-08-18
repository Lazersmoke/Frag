{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Purity.Client.Data where

import Data.Access

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

data ServerMessage = NameQuery | LobbyUpdate [LobbyEntry] | GameUpdate [Object] [(String,Object)] deriving Show

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
  _mouseCallback :: GLFW.MouseButtonCallback,
  _mousePosCallback :: GLFW.CursorPosCallback
  }

buildMode :: (GLFW.Window -> IO ()) -> GLFW.KeyCallback -> GLFW.MouseButtonCallback -> GLFW.CursorPosCallback -> Mode
buildMode draw key mouse mousep = Mode {
  _drawFunction = draw,
  _keyboardCallback = key,
  _mouseCallback = mouse,
  _mousePosCallback = mousep
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

data CursorPosCallback = CursorPosCallback
instance Access Mode GLFW.CursorPosCallback CursorPosCallback where
  grab _ = _mousePosCallback
  lift _ f m = m {_mousePosCallback = f $ _mousePosCallback m} 

data LogLevel = Log | Warn | Error

data Object = Object {
  _pos :: Vector,
  _size :: Vector,
  _dir :: Vector
  } deriving Show

emptyObject :: Object
emptyObject = Object {_pos = 0, _size = 0, _dir = 0}

data ObjectAttr = Position | Size | FacingDirection
instance Access Object Vector ObjectAttr where
  grab Position = _pos
  grab Size = _size
  grab FacingDirection = _dir
  lift Position f o = o {_pos = f $ _pos o}
  lift Size f o = o {_size = f $ _size o}
  lift FacingDirection f o = o {_dir = f $ _dir o}


data RenderPlane = RenderPlane Vector Vector Vector deriving Show

data Vector = Vector Double Double Double

emptyVector :: Vector
emptyVector = 0

unitVector :: Vector
unitVector = emptyVector + 1

data Direction = X | Y | Z

instance Num Vector where
  (+) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
  (*) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)
  abs (Vector x y z) = Vector (abs x) (abs y) (abs z)
  signum (Vector x y z) = Vector (signum x) (signum y) (signum z)
  negate (Vector x y z) = Vector (negate x) (negate y) (negate z)
  fromInteger i = Vector (fromInteger i) (fromInteger i) (fromInteger i)

instance Access Vector Double Direction where
  grab X (Vector x _ _) = x
  grab Y (Vector _ y _) = y
  grab Z (Vector _ _ z) = z
  lift X f (Vector x y z) = Vector (f x) y z
  lift Y f (Vector x y z) = Vector x (f y) z
  lift Z f (Vector x y z) = Vector x y (f z)

instance Show Vector where
  show (Vector x y z) = "<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

toGLVec :: Vector -> GL.Vector3 GL.GLdouble
toGLVec (Vector x y z) = GL.Vector3 x y z

toGLVert :: Vector -> GL.Vertex3 GL.GLdouble
toGLVert (Vector x y z) = GL.Vertex3 x y z

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE PhantomTypes #-}
module Purity.Data where

import Prelude hiding ((.),id)
import Control.Category
import Data.Access

---------------------
-- # ServerState # --
---------------------

-- What phase is the game in?
data ServerPhase = Lobby | Loading | Playing deriving (Show,Eq)

-- A tick is an integer
type Tick = Integer
data ServerState = ServerState {
  _world :: World,
  _players :: [Player],
  _objects :: [Object],
  _phase :: ServerPhase,
  _gameRules :: GameRules,
  _currentTick :: Tick
  }deriving (Eq)

instance Show ServerState where
  show s = 
    "{ServerState " ++ show (phase ~>> s) ++ 
    " on tick " ++ show (currentTick ~>> s) ++
    " with players " ++ show (players ~>> s) ++ "}"

data PhysicsDescriptor = PhysicsDescriptor {
  airAccel :: Double,
  groundAccel :: Double,
  gravityConst :: Double,
  frictionConst :: Double,
  deltaTime :: Double
  }

defaultPhysicsDescriptor :: PhysicsDescriptor
defaultPhysicsDescriptor = PhysicsDescriptor {
  airAccel = 12,
  groundAccel = 12,
  gravityConst = 1,
  frictionConst = 1,
  deltaTime = 0
  }

data GameRules = Rules {
  joinMidGame :: Bool,
  friendlyFire :: Bool
  } deriving (Show,Eq)

-- Default Server State
freshServerState :: ServerState
freshServerState = ServerState 
  World {levelName = "", geometry = []}
  [] -- No Players
  [] -- No Objects
  Lobby -- Start in Lobby
  Rules {
    joinMidGame = False,
    friendlyFire = False
    }
  0 -- Tick 0

world :: ServerState ~> World 
world = Access _world (\f s -> s {_world = f $ _world s})

players :: ServerState ~> [Player]
players = Access _players (\f s -> s {_players = f $ _players s})

objects :: ServerState ~> [Object] 
objects = Access _objects (\f s -> s {_objects = f $ _objects s})

phase :: ServerState ~> ServerPhase 
phase = Access _phase (\f s -> s {_phase = f $ _phase s})

gameRules :: ServerState ~> GameRules 
gameRules = Access _gameRules (\f s -> s {_gameRules = f $ _gameRules s})

currentTick :: ServerState ~> Tick
currentTick = Access _currentTick (\f s -> s {_currentTick = f $ _currentTick s})

-- Additional accessor for a specific player (by id) on a serverstate
specificPlayer :: Identifier -> ServerState ~> Player
specificPlayer pIdent = playerListToPlayer . players
  where
    getThePlayer = head . filter ((==pIdent) . grab ident)
    playerListToPlayer = Access getThePlayer (\f p -> (f (getThePlayer p):) . filter (/= getThePlayer p) $ p)

cmdObject :: Command -> ServerState ~> Object
cmdObject uc = object . specificPlayer (cmdId ~>> uc) 

----------------
-- # Player # --
----------------

type ReadyStatus = Bool
-- What is the player's status
data PlayerStatus = InGame | Respawning | InLobby | Lost deriving (Show,Eq)

type Identifier = String
data Player = Player {
  _ident :: Identifier,
  _name :: String,
  _status :: PlayerStatus,
  _object :: Object,
  _ready :: ReadyStatus
  } deriving Eq
  -- Note: Eq is derived, so players with different positions and same ident are different
ident :: Player ~> Identifier 
ident = Access _ident (\f p -> p {_ident = f $ _ident p})

name :: Player ~> String 
name = Access _name (\f p -> p {_name = f $ _name p})

status :: Player ~> PlayerStatus 
status = Access _status (\f p -> p {_status = f $ _status p})

object :: Player ~> Object 
object = Access _object (\f p -> p {_object = f $ _object p})

ready :: Player ~> ReadyStatus 
ready = Access _ready (\f p -> p {_ready = f $ _ready p})

instance Show Player where
  show p = "{Player " ++ show (name ~>> p) ++ "/" ++ show (status ~>> p) ++ "/" ++ show (ready ~>> p) ++ "}" -- Object: "<|" ++ show (object ~>> p) ++ "|>"

-------------------
-- # Threading # --
-------------------

data NetworkSide = Server | Client deriving (Eq,Show)

data Command = Command {
  _command :: String, -- The Command
  _cmdId :: Identifier, -- The Identifier of the source player
  _source :: NetworkSide
  } deriving (Eq,Show)

mkCommand :: String -> Identifier -> NetworkSide -> Command
mkCommand = Command 

emptyCommand :: NetworkSide -> Command 
emptyCommand = mkCommand "" "" 

command :: Command ~> String 
command = Access _command (\f u -> u {_command = f $ _command u})

cmdId :: Command ~> Identifier 
cmdId = Access _cmdId (\f u -> u {_cmdId = f $ _cmdId u})

source :: Command ~> NetworkSide
source = Access _source (\f u -> u {_source = f $ _source u})

------------------------------
-- # Object (for physics) # --
------------------------------

data Object = Object { 
  _pos :: Position, -- Position
  _size :: Direction, -- Size
  _vel :: Velocity, -- Velocity
  _angles :: Vector, -- Angles (YPR
  _wish :: Direction, -- Wish
  _mode :: PhysicsMode -- Mode
  } deriving Eq

pos :: Object ~> Vector 
pos = Access _pos (\f o -> o {_pos = f $ _pos o})

size :: Object ~> Vector 
size = Access _size (\f o -> o {_size = f $ _size o})

vel :: Object ~> Vector 
vel = Access _vel (\f o -> o {_vel = f $ _vel o})

angles :: Object ~> Vector 
angles = Access _angles (\f o -> o {_angles = f $ _angles o})

yaw :: Object ~> VectorComp 
yaw = vecX . angles

pitch :: Object ~> VectorComp 
pitch = vecY . angles

roll :: Object ~> VectorComp 
roll = vecZ . angles

wish :: Object ~> Vector 
wish = Access _wish (\f o -> o {_wish = f $ _wish o})

mode :: Object ~> PhysicsMode 
mode = Access _mode (\f o -> o {_mode = f $ _mode o})

data PhysicsMode = OnGround | InAir deriving (Show, Eq)-- | OnLadder | InWater
instance Show Object where
  show obj = 
    "{Pos: " ++ show (pos ~>> obj)
    ++ ", Size: " ++ show (size ~>> obj)
    ++ ", Vel: " ++ show (vel ~>> obj)
    ++ ", Yaw: " ++ show (yaw ~>> obj)
    ++ ", Pitch: " ++ show (pitch ~>> obj)
    ++ ", Roll: " ++ show (roll ~>> obj)
    ++ ", Wish: " ++ show (wish ~>> obj)
    ++ ", Mode: " ++ show (mode ~>> obj)
    ++ "}"

emptyObject :: Object
emptyObject = Object 
  emptyVector
  emptyVector
  emptyVector
  emptyVector
  emptyVector
  InAir

oneCube :: Object
oneCube = (size >@> 1) emptyObject

objAABB :: Object -> AABB
objAABB o = (pos ~>> o, pos ~>> o + size ~>> o)

---------------------
-- # World Plane # --
---------------------

data World = World {
  levelName :: String,
  geometry :: [WorldPlane]
  } deriving (Show, Eq)
data WorldPlane = WorldPlane (Vector,Vector,Vector) deriving (Show, Eq)

-----------------
-- # Vectors # --
-----------------

-- Special Vector Names
type Position = Vector 
type Direction = Vector 
type Velocity = Vector
type AABB = (Vector,Vector)

type VectorComp = Double
-- Vector is a triple of doubles
newtype Vector = Vector (VectorComp,VectorComp,VectorComp) deriving (Eq)

instance Show Vector where
  show (Vector (x,y,z)) = "<" ++ show x ++ " " ++ show y ++ " " ++ show z ++ ">"

instance Num Vector where
  (+) (Vector (ax,ay,az)) (Vector (bx,by,bz)) = Vector (ax+bx,ay+by,az+bz)
  (*) (Vector (ax,ay,az)) (Vector (bx,by,bz)) = Vector (ax*bx,ay*by,az*bz)
  negate (Vector (a,b,c)) = Vector (-a,-b,-c)
  abs (Vector (a,b,c)) = Vector (abs a,abs b,abs c)
  signum (Vector (a,b,c)) = Vector (signum a, signum b, signum c)
  fromInteger a = Vector (fromInteger a,fromInteger a,fromInteger a)

emptyVector :: Vector 
emptyVector = 0

unitVector :: Vector 
unitVector = 1

unitVectors :: [Vector]
unitVectors =
  [Vector (1,0,0)
  ,Vector (0,1,0)
  ,Vector (0,0,1)
  ]

vecSum :: Vector -> VectorComp
vecSum (Vector (x,y,z)) = x+y+z

aabbCorners :: AABB -> [Vector]
aabbCorners (Vector (ax,ay,az), Vector (bx,by,bz)) =
  [Vector (ax,ay,az)
  ,Vector (bx,ay,az)
  ,Vector (ax,by,az)
  ,Vector (bx,by,az)
  ,Vector (ax,ay,bz)
  ,Vector (bx,ay,bz)
  ,Vector (ax,by,bz)
  ,Vector (bx,by,bz)
  ]

-- Vector Accessors
vecX :: Vector ~> VectorComp 
vecX = Access (\(Vector (x,_,_)) -> x) (\f (Vector (x,y,z)) -> Vector (f x,y,z))

vecY :: Vector ~> VectorComp 
vecY = Access (\(Vector (_,y,_)) -> y) (\f (Vector (x,y,z)) -> Vector (x,f y,z))

vecZ :: Vector ~> VectorComp 
vecZ = Access (\(Vector (_,_,z)) -> z) (\f (Vector (x,y,z)) -> Vector (x,y,f z))

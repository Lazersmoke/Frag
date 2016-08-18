{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE PhantomTypes #-}
module Purity.Server.Data where

import Data.Access
import Data.List.Split

(~>) :: b -> a -> Compose a b
(~>) = flip Compose 

---------------------
-- # ServerState # --
---------------------

-- What phase is the game in?
data ServerPhase = Lobby | Playing deriving (Show,Eq)

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
    "{ServerState " ++ show (Status ~>> s) ++ 
    " on tick " ++ show (CurrentTick ~>> s) ++
    " with players " ++ show (Players ~>> s) ++ "}"

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

data GameRules = GameRules {
  joinMidGame :: Bool,
  friendlyFire :: Bool
  } deriving (Show,Eq)

-- Default Server State
freshServerState :: ServerState
freshServerState = ServerState 
  World {_levelName = "", _geometry = []}
  [] -- No Players
  [] -- No Objects
  Lobby -- Start in Lobby
  GameRules {
    joinMidGame = False,
    friendlyFire = False
    }
  0 -- Tick 0

data AccessWorld = AccessWorld
instance Access ServerState World AccessWorld where
  grab _ = _world
  lift _ f s = s {_world = f $ _world s}

data Players = Players
instance Access ServerState [Player] Players where
  grab _ = _players
  lift _ f s = s {_players = f $ _players s}

instance Access ServerState [Object] ObjectA where
  grab _ = _objects
  lift _ f s = s {_objects = f $ _objects s}

instance Access ServerState ServerPhase Status where
  grab _ = _phase
  lift _ f s = s {_phase = f $ _phase s}

data Rules = Rules
instance Access ServerState GameRules Rules where
  grab _ = _gameRules
  lift _ f s = s {_gameRules = f $ _gameRules s}

data CurrentTick = CurrentTick
instance Access ServerState Tick CurrentTick where
  grab _ = _currentTick
  lift _ f s = s {_currentTick = f $ _currentTick s}

-- Additional accessor for a specific player (by id) on a serverstate
data SpecificPlayer = Specific Identifier
instance Access ServerState Player SpecificPlayer where
  grab (Specific ident) = getPlayerByIdentity ident . grab Players
  lift (Specific ident) f = Players >&> \p -> ((f (getPlayerByIdentity ident p):) . filter (/= getPlayerByIdentity ident p)) p

getPlayerByIdentity :: Identifier -> [Player] -> Player
getPlayerByIdentity i = head . filter ((==i) . grab Identity)

data CmdObject = CmdObject Command
instance Access ServerState Object CmdObject where
  grab (CmdObject uc) = grab ObjectA . grab (Specific (Identity ~>> uc))
  lift (CmdObject uc) f = Specific (Identity ~>> uc) >&> ObjectA >&> f

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
data Identity = Identity
instance Access Player Identifier Identity where
  grab _ = _ident
  lift _ f p = p {_ident = f $ _ident p}

data Name = Name
instance Access Player String Name where
  grab _ = _name
  lift _ f p = p {_name = f $ _name p}

data Status = Status
instance Access Player PlayerStatus Status where
  grab _ = _status
  lift _ f p = p {_status = f $ _status p}

data ObjectA = ObjectA
instance Access Player Object ObjectA where
  grab _ = _object
  lift _ f p = p {_object = f $ _object p}

data Ready = Ready
instance Access Player ReadyStatus Ready where
  grab _ = _ready
  lift _ f p = p {_ready = f $ _ready p}

instance Show Player where
  show p = "{Player " ++ show (Name ~>> p) ++ "/" ++ show (Status ~>> p) ++ "/" ++ show (Ready ~>> p) ++ "}" -- Object: "<|" ++ show (object ~>> p) ++ "|>"

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

data CommandText = CommandText
instance Access Command String CommandText where
  grab _ = _command
  lift _ f u = u {_command = f $ _command u}

instance Access Command Identifier Identity where
  grab _ = _cmdId
  lift _ f u = u {_cmdId = f $ _cmdId u}

data SourceSide = SourceSide
instance Access Command NetworkSide SourceSide where
  grab _ = _source
  lift _ f u = u {_source = f $ _source u}

------------------------------
-- # Object (for physics) # --
------------------------------

data Object = Object { 
  _pos :: Vector, -- Position
  _size :: Vector, -- Size
  _vel :: Vector, -- Velocity
  _angles :: Vector, -- Angles (YPR
  _wish :: Vector, -- Wish
  _mode :: PhysicsMode -- Mode
  } deriving Eq

data ObjectAttribute = Position | Size | Velocity | Angles | Wish
instance Access Object Vector ObjectAttribute where
  grab Position = _pos
  grab Size = _size
  grab Velocity = _vel
  grab Angles = _angles
  grab Wish = _wish
  lift Position f o = o {_pos = f $ _pos o}
  lift Size f o = o {_size = f $ _size o}
  lift Velocity f o = o {_vel = f $ _vel o}
  lift Angles f o = o {_angles = f $ _angles o}
  lift Wish f o = o {_wish = f $ _wish o}

yaw :: Compose Direction ObjectAttribute
yaw = Compose X Angles

pitch :: Compose Direction ObjectAttribute
pitch = Compose Y Angles

roll :: Compose Direction ObjectAttribute
roll = Compose Z Angles

data PhysicsMode = OnGround | InAir deriving (Show, Eq)-- | OnLadder | InWater
instance Access Object PhysicsMode Status where
  grab _ = _mode
  lift _ f o = o {_mode = f $ _mode o}

instance Show Object where
  show obj = 
    "{Pos: " ++ show (Position ~>> obj)
    ++ ", Size: " ++ show (Size ~>> obj)
    ++ ", Vel: " ++ show (Velocity ~>> obj)
    ++ ", Yaw: " ++ show (Angles ~> X ~>> obj)
    ++ ", Pitch: " ++ show (Angles ~> Y ~>> obj)
    ++ ", Roll: " ++ show (Angles ~> Z ~>> obj)
    ++ ", Wish: " ++ show (Wish ~>> obj)
    ++ ", Mode: " ++ show (Status ~>> obj)
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
oneCube = (Size >@> 1) emptyObject

objAABB :: Object -> AABB
objAABB o = (Position ~>> o, Position ~>> o + Size ~>> o)

---------------------
-- # World Plane # --
---------------------

data World = World {
  _levelName :: String,
  _geometry :: [WorldPlane]
  } deriving (Show, Eq)
data WorldPlane = WorldPlane (Vector,Vector,Vector) deriving (Show, Eq)

data Geometry = Geometry
instance Access World [WorldPlane] Geometry where
  grab _ = _geometry
  lift _ f w = w {_geometry = f $ _geometry w}

instance Access World String Name where
  grab _ = _levelName
  lift _ f w = w {_levelName = f $ _levelName w}

-----------------
-- # Vectors # --
-----------------

-- Special Vector Names
type AABB = (Vector,Vector)

type VectorComp = Double
-- Vector is a triple of doubles
newtype Vector = Vector (VectorComp,VectorComp,VectorComp) deriving (Eq)

instance Show Vector where
  show (Vector (x,y,z)) = "<" ++ showNum x ++ "," ++ showNum y ++ "," ++ showNum z ++ ">"
    where
      showNum n = head . splitOn "e" $ show ((fromIntegral . round) (n * 100) / 100)

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
data Direction = X | Y | Z
instance Access Vector VectorComp Direction where
  grab X (Vector (x,_,_)) = x
  grab Y (Vector (_,y,_)) = y
  grab Z (Vector (_,_,z)) = z
  lift X f (Vector (x,y,z)) = Vector (f x,y,z)
  lift Y f (Vector (x,y,z)) = Vector (x,f y,z)
  lift Z f (Vector (x,y,z)) = Vector (x,y,f z)

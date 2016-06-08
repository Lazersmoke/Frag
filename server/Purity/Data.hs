{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Purity.Data where

import qualified Network.WebSockets as WS
import Data.List

----------------
-- # Access # --
----------------

type Access prop state = (prop -> prop) -> state -> (prop, state)

grabf :: Access prop state -> (prop -> prop) -> state -> prop
grabf a f m = fst $ a f m 

grab :: Access prop state -> state -> prop
grab a m = fst $ a id m

change :: Access prop state -> (prop -> prop) -> state -> state
change a f m = snd $ a f m

changeMap :: Functor a => Access (a prop) state -> (prop -> prop) -> state -> state
changeMap a f m = snd $ a (fmap f) m

set :: Access prop state -> prop -> state -> state
set a n m = snd $ a (const n) m

-- players ~>> ss
(~>>) :: Access p s -> s -> p
(~>>) = grab

-- (phase >@> phase') ss
(>@>) :: Access p s -> p -> s -> s
(>@>) = set

-- (currentTick >&> (+1)) ss
(>&>) :: Access p s -> (p -> p) -> s -> s
(>&>) = change

---------------------
-- # ServerState # --
---------------------

-- What phase is the game in?
data ServerPhase = Lobby | Loading | Playing deriving (Show,Eq)

-- A tick is an integer
type Tick = Integer
data ServerState = ServerState World [Player] [Object] ServerPhase GameRules Tick deriving (Show,Eq)

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

world :: Access World ServerState
world f (ServerState w p o ph r c) = (f w, ServerState (f w) p o ph r c)

players :: Access [Player] ServerState
players f (ServerState w p o ph r c) = (f p, ServerState w (f p) o ph r c)

objects :: Access [Object] ServerState
objects f (ServerState w p o ph r c) = (f o, ServerState w p (f o) ph r c)

phase :: Access ServerPhase ServerState
phase f (ServerState w p o ph r c) = (f ph, ServerState w p o (f ph) r c)

gameRules :: Access GameRules ServerState
gameRules f (ServerState w p o ph r c) = (f r, ServerState w p o ph (f r) c)

currentTick :: Access Tick ServerState
currentTick f (ServerState w p o ph r c) = (f c, ServerState w p o ph r (f c))

-- Add a player to an existing ServerState
addPlayer :: Player -> ServerState -> ServerState
addPlayer pla = change players (pla :) 

-- Drop a player
dropPlayer :: Player -> ServerState -> ServerState
dropPlayer pla = change players (delete pla)

-- Change the first player to the second one
modifyPlayer :: Player -> Player -> ServerState -> ServerState
modifyPlayer old new = change players ((new:) . delete old) 

-- Lifts map over player to work on ServerState's
transformPlayers :: (Player -> Player) -> ServerState -> ServerState
transformPlayers = changeMap players

-- Lifts map over objects to work on ServerState's
transformObjects :: (Object -> Object) -> ServerState -> ServerState
transformObjects = changeMap objects

----------------
-- # Player # --
----------------

type ReadyStatus = Bool
-- What is the player's status
data PlayerStatus = InGame | Respawning | InLobby | Lost deriving (Show,Eq)

data Player = Player {
  name :: String, -- Player's Name
  connection :: WS.Connection, -- Player's Connection
  status :: PlayerStatus, -- Players current state
  object :: Object,
  ready :: ReadyStatus,
  userCmds :: [UserCommand] -- UCs associated with this player
  }

instance Show Player where
  show p = name p ++ "/" ++ (show . status) p ++ "<|" ++ show (object p) ++ "|>"

instance Eq Player where
  (==) a b = name a == name b

data UserCommand = UserCommand {
  tick :: Tick, -- Tick Number
  command :: String -- Keys pressed
  }

-- Set a player's status
setStatus :: PlayerStatus -> Player -> Player
setStatus ps pla = pla {status = ps}

-- Set a player's readiness
setReady :: ReadyStatus -> Player -> Player
setReady rs pla = pla {ready = rs}

-- Set a player's object
setObject :: Object -> Player -> Player
setObject obj pla = pla {object = obj}

-- Transform a player's object
transformObject :: (Object -> Object) -> Player -> Player
transformObject f pla = setObject (f . object $ pla) pla

-- Add a UC to a player
addUC :: UserCommand -> Player -> Player
addUC uc pla = pla {userCmds = uc : userCmds pla}

------------------------------
-- # Object (for physics) # --
------------------------------

data Object = Object {
  pos :: Position,
  size :: Direction,
  vel :: Velocity,
  yaw :: VectorComp,
  pitch :: VectorComp,
  roll :: VectorComp,
  wish :: Direction,
  mode :: PhysicsMode
  } deriving Eq

data PhysicsMode = OnGround | InAir deriving (Show, Eq)-- | OnLadder | InWater
instance Show Object where
  show obj = 
    "{Pos: " ++ show (pos obj) 
    ++ ", Size: " ++ show (size obj) 
    ++ ", Vel: " ++ show (vel obj) 
    ++ ", Yaw: " ++ show (yaw obj) 
    ++ ", Pitch: " ++ show (pitch obj) 
    ++ ", Roll: " ++ show (roll obj) 
    ++ ", Wish: " ++ show (wish obj) 
    ++ ", Mode: " ++ show (mode obj) 
    ++ "}"

emptyObject :: Object
emptyObject = Object {
  pos = emptyVector,
  size = emptyVector,
  vel = emptyVector,
  yaw = 0,
  pitch = 0,
  roll = 0,
  wish = emptyVector,
  mode = InAir
  }

oneCube :: Object
oneCube = emptyObject {size = 1}

transformWish :: (Direction -> Direction) -> Object -> Object
transformWish f obj = obj {wish = f $ wish obj}

objAABB :: Object -> AABB
objAABB o = (pos o, pos o + size o)

-- Set an object's Physics Mode
setMode :: PhysicsMode -> Object -> Object
setMode m obj = obj {mode = m}

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

-- Get components
vecX :: Vector -> VectorComp
vecX (Vector (a,_,_)) = a

setVecX :: VectorComp -> Vector -> Vector
setVecX a (Vector (_,b,c)) = Vector (a,b,c)

vecY :: Vector -> VectorComp
vecY (Vector (_,a,_)) = a

setVecY :: VectorComp -> Vector -> Vector
setVecY b (Vector (a,_,c)) = Vector (a,b,c)

vecZ :: Vector -> VectorComp
vecZ (Vector (_,_,a)) = a

setVecZ :: VectorComp -> Vector -> Vector
setVecZ c (Vector (a,b,_)) = Vector (a,b,c)

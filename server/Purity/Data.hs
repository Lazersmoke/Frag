{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Purity.Data where

import qualified Network.WebSockets as WS

----------------
-- # Access # --
----------------

type Access prop state = (prop -> prop) -> state -> (prop, state)

grabf :: Access prop state -> (prop -> result) -> state -> result
grabf a f m = f (grab a m)

grabShow :: Show prop => Access prop state -> state -> String
grabShow a m = show (grab a m)

grab :: Access prop state -> state -> prop
grab a m = fst $ a id m

change :: Access prop state -> (prop -> prop) -> state -> state
change a f m = snd $ a f m

changeMap :: Functor a => Access (a prop) state -> (prop -> prop) -> state -> state
changeMap a f m = snd $ a (fmap f) m

set :: Access prop state -> prop -> state -> state
set a n m = snd $ a (const n) m

-- players ~>> ss
-- Or
-- ss <<~ players
(~>>) :: Access p s -> s -> p
(~>>) = grab
infixr 9 ~>>

(<<~) :: s -> Access p s -> p
(<<~) = flip (~>>)
infixl 9 <<~ 

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
data ServerState = ServerState World [Player] [Object] ServerPhase GameRules Tick [UserCommand] deriving (Show,Eq)

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
  [] -- No User Commands

world :: Access World ServerState
world f (ServerState w p o ph r c u) = (f w, ServerState (f w) p o ph r c u)

players :: Access [Player] ServerState
players f (ServerState w p o ph r c u) = (f p, ServerState w (f p) o ph r c u)

objects :: Access [Object] ServerState
objects f (ServerState w p o ph r c u) = (f o, ServerState w p (f o) ph r c u)

phase :: Access ServerPhase ServerState
phase f (ServerState w p o ph r c u) = (f ph, ServerState w p o (f ph) r c u)

gameRules :: Access GameRules ServerState
gameRules f (ServerState w p o ph r c u) = (f r, ServerState w p o ph (f r) c u)

currentTick :: Access Tick ServerState
currentTick f (ServerState w p o ph r c u) = (f c, ServerState w p o ph r (f c) u)

userCmds :: Access [UserCommand] ServerState
userCmds f (ServerState w p o ph r c u) = (f u, ServerState w p o ph r c (f u))

----------------
-- # Player # --
----------------

type ReadyStatus = Bool
-- What is the player's status
data PlayerStatus = InGame | Respawning | InLobby | Lost deriving (Show,Eq)

data Player = Player String WS.Connection PlayerStatus Object ReadyStatus [UserCommand] 

name :: Access String Player
name f (Player n c s o r u) = (f n, Player (f n) c s o r u)

connection :: Access WS.Connection Player
connection f (Player n c s o r u) = (f c, Player n (f c) s o r u)

status :: Access PlayerStatus Player
status f (Player n c s o r u) = (f s, Player n c (f s) o r u)

object :: Access Object Player
object f (Player n c s o r u) = (f o, Player n c s (f o) r u)

ready :: Access ReadyStatus Player
ready f (Player n c s o r u) = (f r, Player n c s o (f r) u)

instance Show Player where
  show p = name ~>> p ++ "/" ++ grabShow status p ++ "<|" ++ grabShow object p ++ "|>"

instance Eq Player where
  (==) a b = name ~>> a == name ~>> b

data UserCommand = UserCommand
  Tick -- Tick Number
  String -- The Command
  String -- The Name of the source player
  deriving (Eq,Show)

tick :: Access Tick UserCommand
tick f (UserCommand t c p) = (f t,UserCommand (f t) c p)

command :: Access String UserCommand
command f (UserCommand t c p) = (f c,UserCommand t (f c) p)

sourcePlayerName :: Access String UserCommand
sourcePlayerName f (UserCommand t c p) = (f p,UserCommand t c (f p))

------------------------------
-- # Object (for physics) # --
------------------------------

data Object = Object 
  Position -- Position
  Direction -- Size
  Velocity -- Velocity
  Vector -- Angles (YPR
  Direction -- Wish
  PhysicsMode -- Mode
  deriving Eq

pos :: Access Vector Object
pos     f (Object p s v a w m) = (f p, Object (f p) s v a w m)

size :: Access Vector Object
size    f (Object p s v a w m) = (f s, Object p (f s) v a w m)

vel :: Access Vector Object
vel     f (Object p s v a w m) = (f v, Object p s (f v) a w m)

angles :: Access Vector Object
angles  f (Object p s v a w m) = (f a, Object p s v (f a) w m)

yaw :: Access VectorComp Object
yaw     f (Object p s v a w m) = (grabf vecX f a, Object p s v ((vecX >&> f) a) w m)

pitch :: Access VectorComp Object
pitch   f (Object p s v a w m) = (grabf vecY f a, Object p s v ((vecY >&> f) a) w m)

roll :: Access VectorComp Object
roll    f (Object p s v a w m) = (grabf vecZ f a, Object p s v ((vecZ >&> f) a) w m)

wish :: Access Vector Object
wish    f (Object p s v a w m) = (f w, Object p s v a (f w) m)

mode :: Access PhysicsMode Object
mode    f (Object p s v a w m) = (f m, Object p s v a w (f m))

data PhysicsMode = OnGround | InAir deriving (Show, Eq)-- | OnLadder | InWater
instance Show Object where
  show obj = 
    "{Pos: " ++ grabShow pos obj 
    ++ ", Size: " ++ grabShow size obj 
    ++ ", Vel: " ++ grabShow vel obj 
    ++ ", Yaw: " ++ grabShow yaw obj 
    ++ ", Pitch: " ++ grabShow pitch obj 
    ++ ", Roll: " ++ grabShow roll obj
    ++ ", Wish: " ++ grabShow wish obj 
    ++ ", Mode: " ++ grabShow mode obj 
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
vecX :: Access VectorComp Vector 
vecX f (Vector (x,y,z)) = (f x,Vector (f x,y,z))

vecY :: Access VectorComp Vector 
vecY f (Vector (x,y,z)) = (f y,Vector (x,f y,z))

vecZ :: Access VectorComp Vector 
vecZ f (Vector (x,y,z)) = (f z,Vector (x,y,f z))

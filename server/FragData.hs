{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FragData where

import qualified Network.WebSockets as WS
import Control.Concurrent
import Control.Monad.Reader
import Data.List

----------------------------
-- # Monad Transformers # --
----------------------------

-- For handling incoming connections
newtype ConnectionT a = MkConnectionT {
  unwrapConnectionT :: ReaderT (MVar ServerState) IO a
} deriving (Functor, Applicative, Monad, MonadReader (MVar ServerState), MonadIO)

-- Unwrap a ConnectionT be running its reader and turning into regular IO
runConnectionT :: MVar ServerState -> ConnectionT a -> IO a
runConnectionT mvar k = runReaderT (unwrapConnectionT k) mvar

-- For game logic
newtype GameCoreT a = MkGameCoreT {
  unwrapGameCoreT :: ReaderT (MVar ServerState) IO a
} deriving (Functor, Applicative, Monad, MonadReader (MVar ServerState), MonadIO)

-- Unwrap a GameCoreT be running its reader and turning into regular IO
runGameCoreT :: MVar ServerState -> GameCoreT a -> IO a
runGameCoreT mvar k = runReaderT (unwrapGameCoreT k) mvar

---------------------
-- # ServerState # --
---------------------

-- What phase is the game in?
data ServerPhase = Lobby | Loading | Playing deriving (Show,Eq)

-- A tick is an integer
type Tick = Integer
data ServerState = ServerState {
  players :: [Player],
  objects :: [Object],
  phase :: ServerPhase,
  rules :: GameRules,
  currentTick :: Tick
  } deriving (Show,Eq)

data GameRules = Rules {
  joinMidGame :: Bool,
  friendlyFire :: Bool
  } deriving (Show,Eq)

-- Default Server State
freshServerState :: ServerState
freshServerState = ServerState {
  players = [],
  objects = [],
  phase = Lobby,
  rules = Rules {
    joinMidGame = False,
    friendlyFire = False
    },
  currentTick = 0
  }

setPhase :: ServerPhase -> ServerState -> ServerState
setPhase sp ss = ss {phase = sp}
-- Add a player to an existing ServerState
addPlayer :: Player -> ServerState -> ServerState
addPlayer pla ss = ss {players = pla : players ss}

-- Drop a player
dropPlayer :: Player -> ServerState -> ServerState
dropPlayer pla ss = ss {players = delete pla $ players ss}

-- Change the first player to the second one
modifyPlayer :: Player -> Player -> ServerState -> ServerState
modifyPlayer old new ss = ss {players = new : delete old (players ss)}

-- Lifts map over player to work on ServerState's
transformPlayers :: (Player -> Player) -> ServerState -> ServerState
transformPlayers f ss = ss {players = map f $ players ss}

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
  show p = name p ++ "/" ++ (show . status) p

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
  dir :: Direction,
  wish :: Direction
  } deriving Eq

instance Show Object where
  show obj = "{Pos: " ++ show (pos obj) ++ ", Size: " ++ show (size obj) ++  ", Vel: " ++ show (vel obj) ++ ", Dir: " ++ show (dir obj) ++ ", Wish: " ++ show (wish obj) ++ "}"

emptyObject :: Object
emptyObject = Object {
  pos = emptyVector,
  size = emptyVector,
  vel = emptyVector,
  dir = emptyVector,
  wish = emptyVector
  }

oneCube :: Object
oneCube = emptyObject {size = 1}

---------------------
-- # World Plane # --
---------------------

data World = World {
  geometry :: [WorldPlane]
  }
data WorldPlane = WorldPlane (Vector,Vector,Vector)

normalWP :: WorldPlane -> Vector 
normalWP (WorldPlane (v1,v2,v3)) = a
  where
    p1 = v2 - v1
    p2 = v3 - v1
    vx = vecY p1 * vecZ p2 - vecZ p1 * vecY p2
    vy = vecZ p1 * vecX p2 - vecX p1 * vecZ p2
    vz = vecX p1 * vecY p2 - vecY p1 * vecX p2
    v = Vector (vx,vy,vz)
    abssum = abs vx + abs vy + abs vz
    a = scale (1/abssum) v

repairWPIntersection :: Object -> WorldPlane -> Object
repairWPIntersection obj wp = obj {vel = vel obj - scale (dotProduct (vel obj) (normalWP wp) / magnitude (normalWP wp)) (normalWP wp)}

-----------------
-- # Vectors # --
-----------------

-- Special Vector Names
type Position = Vector 
type Direction = Vector 
type Velocity = Vector

type VectorComp = Double
-- Vector is a triple of doubles
newtype Vector = Vector (VectorComp,VectorComp,VectorComp) deriving (Show,Eq)

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

magnitude :: Vector -> VectorComp
magnitude (Vector (a,b,c)) = sqrt $ a**2 + b**2 + c**2

-- Scale by a number (prefix synonym for (*))
scale :: VectorComp -> Vector -> Vector
scale s = (Vector (s,s,s) *) 

dotProduct :: Vector -> Vector -> VectorComp
dotProduct (Vector (ax,ay,az)) (Vector (bx,by,bz)) = (ax*bx)+(ay*by)+(az*bz)

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

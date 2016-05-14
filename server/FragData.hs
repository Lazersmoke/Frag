{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FragData where

import qualified Network.WebSockets as WS
import Control.Concurrent
import Control.Monad.Reader

newtype Num a => Vector a = Vector (a,a,a) deriving (Show,Eq)

instance Num a => Num (Vector a) where
  (+) (Vector (ax,ay,az)) (Vector (bx,by,bz)) = Vector (ax+bx,ay+by,az+bz)
  (*) (Vector (ax,ay,az)) (Vector (bx,by,bz)) = Vector (ax*bx,ay*by,az*bz)
  negate (Vector (a,b,c)) = Vector (-a,-b,-c)
  abs (Vector (a,b,c)) = Vector (abs a,abs b,abs c)
  signum (Vector (a,b,c)) = Vector (signum a, signum b, signum c)
  fromInteger a = Vector (fromInteger a,fromInteger a,fromInteger a)

scale :: Num a => a -> Vector a -> Vector a
scale a (Vector (b,c,d)) = Vector (a*b,a*c,a*d)

vecX :: Num a => Vector a -> a
vecX (Vector (a,_,_)) = a
vecY :: Num a => Vector a -> a
vecY (Vector (_,a,_)) = a
vecZ :: Num a => Vector a -> a
vecZ (Vector (_,_,a)) = a

emptyObject :: Object
emptyObject = Object {
  pos = Vector (0,0,0),
  size = Vector (0,0,0),
  vel = Vector (0,0,0),
  dir = Vector (0,0,0),
  wish = Vector (0,0,0)
  }

oneCube :: Object
oneCube = emptyObject {size = Vector (1,1,1)}

type Position = Vector Double
type Direction = Vector Double
type Velocity = Direction
type ReadyStatus = Bool
type Tick = Integer
data PlayerStage = InGame Object | Respawning | InLobby ReadyStatus | Lost deriving (Show,Eq)

data Object = Object {
  pos :: Position,
  size :: Direction,
  vel :: Velocity,
  dir :: Direction,
  wish :: Direction
  } deriving Eq

instance Show Object where
  show obj = "{Pos: " ++ show (pos obj) ++ ", Size: " ++ show (size obj) ++  ", Vel: " ++ show (vel obj) ++ ", Dir: " ++ show (dir obj) ++ ", Wish: " ++ show (wish obj) ++ "}"

data Player = Player {
  name :: String, -- Player's Name
  connection :: WS.Connection, -- Player's Connection
  playerStage :: PlayerStage, -- Players current state
  userCmds :: [UserCommand] -- UCs associated with this player
  }

instance Show Player where
  show p = name p ++ "/" ++ (show . playerStage) p

instance Eq Player where
  (==) a b = name a == name b

data UserCommand = UserCommand {
  tick :: Tick, -- Tick Number
  command :: String -- Keys pressed
  }

data GameStage = Lobby | Loading | Playing deriving (Show,Eq)

data ServerState = ServerState {
  players :: [Player],
  objects :: [Object],
  stage :: GameStage,
  rules :: GameRules,
  currentTick :: Tick
  } deriving (Show,Eq)

-- Default Server State
freshServerState :: ServerState
freshServerState = ServerState {
  players = [],
  objects = [],
  stage = Lobby,
  rules = Rules {
    joinMidGame = False,
    friendlyFire = False
    },
  currentTick = 0
  }

data GameRules = Rules {
  joinMidGame :: Bool,
  friendlyFire :: Bool
  } deriving (Show,Eq)

newtype ConnectionT a = MkConnectionT {
  unwrapConnectionT :: ReaderT (MVar ServerState) IO a
} deriving (Functor, Applicative, Monad, MonadReader (MVar ServerState), MonadIO)

-- Unwrap a ConnectionT be running its reader and turning into regular IO
runConnectionT :: MVar ServerState -> ConnectionT a -> IO a
runConnectionT mvar k = runReaderT (unwrapConnectionT k) mvar

newtype GameCoreT a = MkGameCoreT {
  unwrapGameCoreT :: ReaderT (MVar ServerState) IO a
} deriving (Functor, Applicative, Monad, MonadReader (MVar ServerState), MonadIO)

-- Unwrap a GameCoreT be running its reader and turning into regular IO
runGameCoreT :: MVar ServerState -> GameCoreT a -> IO a
runGameCoreT mvar k = runReaderT (unwrapGameCoreT k) mvar


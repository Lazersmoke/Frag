{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FragData where

import qualified Network.WebSockets as WS
import Control.Concurrent
import Control.Monad.Reader

type Position = (Double,Double,Double)
type ReadyStatus = Bool
type Tick = Integer
data PlayerStage = InGame Position | Respawning | InLobby ReadyStatus | Lost deriving (Show,Eq)

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
  stage :: GameStage,
  rules :: GameRules,
  currentTick :: Tick
  } deriving (Show,Eq)

-- Default Server State
freshServerState :: ServerState
freshServerState = ServerState {
  players = [],
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


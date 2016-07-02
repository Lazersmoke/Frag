module Purity.Control where

import Purity.Data
import Purity.Physics
import Purity.Util
import Purity.Commands

import Data.Access

import Control.Monad
import Control.Concurrent
import Data.Time.Clock.POSIX
{- Control Flow:
 - incrementTick
 - check players for UCs 
 -
 -}

-- Wraps up main loop to a nice IO ()
startMainLoop :: MVar [UserCommand] -> MVar [ServerCommand] -> ServerState -> IO ()
startMainLoop ucs scs ss = getPOSIXTime >>= mainLoop ucs scs ss

-- TODO: Split into lobby tick, loading tick, playing tick functions
-- Loop that contains all game logic
mainLoop :: MVar [UserCommand] -> MVar [ServerCommand] -> ServerState -> POSIXTime -> IO ()
mainLoop ucs scs ss lastTickStart = do
  -- Get tick start time and calculate dt 
  tickStartTime <- getPOSIXTime 
  let dt = realToFrac $ tickStartTime - lastTickStart
  case phase ~>> ss of
    Lobby -> do
      let ss' = updateReady ss
      -- Start when everyone (at least one) is ready
      if all (ready ~>>) (players ~>> ss') && (not . null $ players ~>> ss')
        -- Start by setting phase to Playing and setting everyone to respawning
        then mainLoop ucs scs (startGame ss') tickStartTime
        else threadDelay 10000 >> mainLoop ucs scs ss' tickStartTime
    Loading -> do 
      threadDelay 10000
      mainLoop ucs scs ss tickStartTime
    Playing -> do
      -- Actually tick the game
      let 
        ticked = 
          incrementTick -- Increment the tick counter
          . doPhysics defaultPhysicsDescriptor {deltaTime = dt} -- Do the physics
          . doUserCmds -- Do all the user actions
          $ ss -- Apply to ServerState
      -- Split to the players and console
      tee
        -- Send it to each player
        tellPlayersState
        -- Debug it to console
        (\_ -> return ()) 
        -- (io . print . map command . concatMap userCmds . players)
        -- Split the ticked version
        ticked
      -- Wait a tick
      threadDelay 10000
      -- Recurse
      mainLoop ucs scs ticked tickStartTime

tellPlayersState :: ServerState -> IO ()
tellPlayersState ss = forM_ (players ~>> ss) $ flip sendMessagePlayer (generateMessage ss)

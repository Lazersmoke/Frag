module Purity.Control where

import Purity.Data
import Purity.Physics
import Purity.Util
import Purity.Commands

import Data.Access

import Control.Concurrent
import Data.Time.Clock.POSIX
{- Control Flow:
 - incrementTick
 - check players for UCs 
 -
 -}

-- Wraps up main loop to a nice IO ()
startMainLoop :: MVar [Command] -> ServerState -> IO ()
startMainLoop commands ss = getPOSIXTime >>= mainLoop commands ss

-- TODO: Split into lobby tick, loading tick, playing tick functions
-- Loop that contains all game logic
mainLoop :: MVar [Command] -> ServerState -> POSIXTime -> IO ()
mainLoop commands ss lastTickStart = do
  -- Get tick start time and calculate dt 
  tickStartTime <- getPOSIXTime 
  -- Calculate dt from tick timings
  let dt = realToFrac $ tickStartTime - lastTickStart
  -- Grab the latest messages
  latestIncomingCommands <- filter ((==Client) . grab source) <$> readMVar commands
  -- Delete them from the MVar
  pModifyMVar_ commands $ filter ((/=Client) . grab source)
  case phase ~>> ss of
    Lobby -> do
      -- Perform only join/ready/unready commands when in lobby (else is discarded)
      let ss' = performLobbyCommands latestIncomingCommands ss
      -- Start when everyone (at least one) is ready
      if all (ready ~>>) (players ~>> ss') && (not . null $ players ~>> ss')
        -- Start by setting phase to Playing and setting everyone to respawning
        then mainLoop commands (startGame ss') tickStartTime
        -- Otherwise wait a bit and loop
        else threadDelay 10000 >> mainLoop commands ss' tickStartTime
    Loading -> do 
      threadDelay 10000
      mainLoop commands ss tickStartTime
    Playing -> do
      -- Actually tick the game
      let 
        ticked = 
          incrementTick -- Increment the tick counter
          . doPhysics defaultPhysicsDescriptor {deltaTime = dt} -- Do the physics
          . performCommands latestIncomingCommands -- Do all the user actions
          $ ss -- Apply to ServerState
      -- Split to the players and console
      tee
        -- Send it to each player
        (tellPlayersState commands)
        -- Debug it to console
        (\_ -> return ()) 
        -- (io . print . map command . concatMap userCmds . players)
        -- Split the ticked version
        ticked
      -- Wait a tick
      threadDelay 10000
      -- Recurse
      mainLoop commands ticked tickStartTime

tellPlayersState :: MVar [Command] -> ServerState -> IO ()
tellPlayersState commands ss = pModifyMVar_ commands (++ map mkMessage (players ~>> ss))
  where mkMessage p = mkCommand (generateMessage ss) (ident ~>> p) Server

module Purity.Server.Control where

import Purity.Server.Data
import Purity.Server.Physics
import Purity.Server.Util
import Purity.Server.Commands

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
  latestIncomingCommands <- filter ((==Client) . grab SourceSide) <$> readMVar commands
  -- Delete them from the MVar
  pModifyMVar_ commands $ filter ((/=Client) . grab SourceSide)
  case Status ~>> ss of
    Lobby -> do
      -- Perform only join/ready/unready commands when in lobby (else is discarded)
      let ss' = performLobbyCommands latestIncomingCommands ss
      -- Start when everyone (at least one) is ready
      tellPlayersState commands ss'
      if all (Ready ~>>) (Players ~>> ss') && (not . null $ Players ~>> ss')
        -- Start by setting phase to Playing and setting everyone to respawning
        then mainLoop commands (startGame ss') tickStartTime
        -- Otherwise wait a bit and loop
        else threadDelay 10000 >> mainLoop commands ss' tickStartTime
    Playing -> do
      -- Actually tick the game
      let 
        ticked = 
          incrementTick -- Increment the tick counter
          . rotatePlayer -- Rotate the player for cinematic effect
          . doPhysics defaultPhysicsDescriptor {deltaTime = dt} -- Do the physics
          . performCommands latestIncomingCommands -- Do all the user actions
          $ ss -- Apply to ServerState
      -- Send it to each player
      tellPlayersState commands ticked
      -- Debug it to console
      print . map (grab Angles . grab ObjectA) . grab Players $ ticked
      -- (print . map command . concatMap userCmds . players)
      -- Wait a tick
      threadDelay 10000
      -- Recurse
      mainLoop commands ticked tickStartTime

--TODO remove this
rotatePlayer :: ServerState -> ServerState
rotatePlayer = liftMap Players (ObjectA ~> yaw >&> (+0.1))

tellPlayersState :: MVar [Command] -> ServerState -> IO ()
tellPlayersState commands ss = pModifyMVar_ commands (++ map mkMessage (Players ~>> ss))
  where 
    mkMessage :: Player -> Command
    mkMessage p = mkCommand (generateMessage ss) (Identity ~>> p) Server

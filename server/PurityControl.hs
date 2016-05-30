module PurityControl where

import PurityData
import PurityPhysics
import PurityUtil
import PurityCommands

import Control.Monad
import Control.Concurrent
import Data.Time.Clock.POSIX
{- Control Flow:
 - incrementTick
 - check players for UCs 
 -
 -}

-- Wraps up main loop to a nice IO ()
mainWrapper :: MVar ServerState -> IO ()
mainWrapper ss = runGameCoreT ss (io getPOSIXTime >>= mainLoop)

-- Loop that contains all game logic
mainLoop :: POSIXTime -> GameCoreT ()
mainLoop lastTickStart = do
  -- Switch on game phase
  ss <- grabState
  tickStartTime <- io getPOSIXTime 
  let dt = realToFrac $ tickStartTime - lastTickStart
  case phase ss of
    Lobby -> do
      transformState $ transformPlayers updateReady
      -- Start when everyone (at least one) is ready
      when (all ready (players ss) && (not . null $ players ss)) 
        -- Start by setting phase to Playing and setting everyone to respawning
        (transformState $ setPhase Playing . transformPlayers (setStatus Respawning))
    Loading -> return ()
    Playing -> do
      transformState (
        -- Increment the tick counter
        incrementTick 
        -- Do the physics
        . doPhysics dt
        -- Do all the user actions
        . doUserCmds
        )
      -- Grab the new state
      grabState >>= tee
        -- Send it to each player
        tellPlayersState
        -- Debug it to console
        (\_ -> return ()) -- (io . print . map command . concatMap userCmds . players)
  io $ threadDelay 10000
  mainLoop tickStartTime

getDeltaTime :: GameCoreT Double
getDeltaTime = return 0.01

tellPlayersState :: ServerState -> GameCoreT ()
tellPlayersState ss = io . forM_ (players ss) $ flip sendMessagePlayer (generateMessage ss)

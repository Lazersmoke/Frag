module FragControl where

import FragData
import FragPhysics
import FragUtil
import FragCommands
import Control.Monad
import Control.Concurrent
{- Control Flow:
 - incrementTick
 - check players for UCs 
 -
 -}

-- Wraps up main loop to a nice IO ()
mainWrapper :: MVar ServerState -> IO ()
mainWrapper ss = runGameCoreT ss mainLoop

-- Loop that contains all game logic
mainLoop :: GameCoreT ()
mainLoop = do
  -- Switch on game phase
  ss <- grabState
  case phase ss of
    Lobby -> do
      transformState $ transformPlayers updateReady
      -- Start when everyone (at least one) is ready
      when (all ready (players ss) && (not . null $ players ss)) 
        -- Start by setting phase to Playing and setting everyone to respawning
        (transformState $ setPhase Playing . transformPlayers (setStatus Respawning))
    Loading -> return ()
    Playing -> do
      deltaTime <- getDeltaTime
      grabState >>= (io . print . map command . concatMap userCmds . players)
      transformState (
        -- Increment the tick counter
        incrementTick 
        -- Do the physics
        . doPhysics deltaTime
        -- Do all the user actions
        . doUserCmds
        )
      -- Grab the new state
      grabState >>= tee
        -- Send it to each player
        tellPlayersState
        -- Debug it to console
        (io . print . map command . concatMap userCmds . players)
  io $ threadDelay 100000
  mainLoop

getDeltaTime :: GameCoreT Double
getDeltaTime = return 0.01

tellPlayersState :: ServerState -> GameCoreT ()
tellPlayersState ss = io . forM_ (players ss) $ flip sendMessagePlayer (generateMessage ss)




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
startMainLoop :: MVar ServerState -> IO ()
startMainLoop ss = getPOSIXTime >>= mainLoop ss

-- Loop that contains all game logic
mainLoop :: MVar ServerState -> POSIXTime -> IO ()
mainLoop mvss lastTickStart = do
  -- Switch on game phase
  ss <- readMVar mvss
  tickStartTime <- getPOSIXTime 
  let dt = realToFrac $ tickStartTime - lastTickStart
  case phase ss of
    Lobby -> do
      modifyMVar_ mvss $ return . transformPlayers updateReady
      -- Start when everyone (at least one) is ready
      when (all ready (players ss) && (not . null $ players ss)) 
        -- Start by setting phase to Playing and setting everyone to respawning
        (modifyMVar_ mvss $ return . setPhase Playing . transformPlayers (setStatus Respawning))
    Loading -> return ()
    Playing -> do
      modifyMVar_ mvss (return
        -- Increment the tick counter
        . incrementTick 
        -- Do the physics
        . doPhysics dt
        -- Do all the user actions
        . doUserCmds
        )
      -- Grab the new state
      readMVar mvss >>= tee
        -- Send it to each player
        tellPlayersState
        -- Debug it to console
        (\_ -> return ()) -- (io . print . map command . concatMap userCmds . players)
  threadDelay 10000
  mainLoop mvss tickStartTime

tellPlayersState :: ServerState -> IO ()
tellPlayersState ss = forM_ (players ss) $ flip sendMessagePlayer (generateMessage ss)

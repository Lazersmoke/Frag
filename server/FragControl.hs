module FragControl where

import FragData
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

updateReady :: Player -> Player
updateReady p
  | "Ready" `elem` map command (userCmds p) = p {ready = True}
  | "Unready" `elem` map command (userCmds p) = p {ready = False}
  | otherwise = p

getDeltaTime :: GameCoreT Double
getDeltaTime = return 0.01


tellPlayersState :: ServerState -> GameCoreT ()
tellPlayersState ss = io . forM_ (players ss) $ flip sendMessagePlayer (generateMessage ss)


doPhysics :: Double -> ServerState -> ServerState
doPhysics dt ss = ss {
  objects = map (collideWithWorld . tickPosition . tickAcceleration) (objects ss),
  players = map (transformObject collideWithWorld . transformObject tickPosition . transformObject tickAcceleration) (players ss)}
  where
    collideWithWorld = listApFold (map (\w x -> if intersectAABBWP (objAABB x) w then repairWPIntersection w x else x) . geometry . world $ ss)
    -- Move all objects to next location, regardless of any collisions
    tickAcceleration = tickObjectAcceleration dt
    tickPosition = tickObjectPosition dt 
      
tickObjectPosition :: Double -> Object -> Object
tickObjectPosition dt obj = obj {pos = scale dt (vel obj) + pos obj}

tickObjectAcceleration :: Double -> Object -> Object
tickObjectAcceleration dt obj = obj {vel = scale accelSpeed wishDir + vel obj}
  where
    -- Break wish vec into components
    wishDir = normalizeVector $ wish obj
    wishSpeed = magnitude $ wish obj
    -- Current speed, as dot product on wishDir
    currSpeed = dotProduct wishDir (vel obj) 
    -- Max speed that we can add
    addSpeed = max (wishSpeed - currSpeed) 0
    -- Actual speed to add
    accelSpeed = min (12 * dt * wishSpeed) addSpeed
{-
 - This code used to deal with Object-Object physics before WPs were a thing. Code is really bad, though
hasCollision :: Object -> [Object] -> Bool
hasCollision obj = any (intersection obj) . delete obj 

resolveCollision :: Object -> [Object] -> Object
resolveCollision obj allObjs = foldl res obj collidedObjs
  where
    collidedObjs = filter (intersection obj) allObjs
    res orig col = 
      let dp = dotProduct (pos orig - pos col) (vel col - vel orig) in
      if dp > 0
        then orig {vel = negate $ vel orig}
        else orig


-- TODO: Actual collision physics
doCollisionPhysics :: Double -> Object -> Object
doCollisionPhysics _ obj = obj {vel = emptyVector}

intersection :: Object -> Object -> Bool 
intersection a b = not $ c vecX || c vecY || c vecZ
  where
    -- Actual Wizardry
    c f = f at < f bo || f bt < f ao
    ao = pos a
    bo = pos b
    at = size a + pos a
    bt = size b + pos b
-}
{-
 - Resolve all object collisions
 - Move each object to next position
 -
 - Do all the steps forward
 - for collisions:
 -   set position right up against
 -   set velocity to zero along surface normal
 -}

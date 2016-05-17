
module FragControl where

import FragData
import FragUtil
import Control.Monad
import Control.Concurrent
import Data.List
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
    Lobby -> 
      -- Start when everyone (at least one) is ready
      when (all ready (players ss) && (not . null $ players ss)) 
        -- Start by setting phase to Playing and setting everyone to respawning
        (transformState $ setPhase Playing . transformPlayers (setStatus Respawning))
    Loading -> return ()
    Playing -> do
      deltaTime <- getDeltaTime
      transformState (
        -- Increment the tick counter
        incrementTick 
        -- Do the physics
        . doPhysics deltaTime
        -- Do all the user actions
        . (\x -> foldl (flip id) x . map performUCs $ players ss) 
        )
      -- Grab the new state
      grabState >>= tee
        -- Send it to each player
        (\x -> forM_ (players x) $ flip sendMessagePlayer (show x))
        -- Debug it to console
        (io . print)
  io $ threadDelay 100000
  mainLoop

getDeltaTime :: GameCoreT Double
getDeltaTime = return 0.1

doPhysics :: Double -> ServerState -> ServerState
doPhysics dt ss = ss {objects = resolveCollisions . updateObjects $ objects ss} -- . playersTransformed 
  where
    -- Move all objects to next location, regardless of any collisions
    updateObjects = map (doObjectPhysics dt) 
    -- Detect and correct collisions
    resolveCollisions objs = map (\x -> switch (resolveCollision x objs) x (hasCollision x objs)) objs
    --nonCollide objs = filter (\x -> not $ any (intersection x) (delete x objs)) objs
      
doObjectPhysics :: Double -> Object -> Object
doObjectPhysics dt obj = obj {pos = scale dt (vel obj) + pos obj}

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

{-
 - Resolve all object collisions
 - Move each object to next position
 -
 - Do all the steps forward
 - for collisions:
 -   set position right up against
 -   set velocity to zero along surface normal
 -}

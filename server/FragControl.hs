
module FragControl where

import FragData
import FragUtil
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
  transformState incrementTick
  ss <- grabState
  forM_ (players ss) (transformState . performUCs)
  deltaTime <- getDeltaTime
  transformState (doPhysics deltaTime)
  grabState >>= io . print

getDeltaTime = return 0.1

doPhysics :: Double -> ServerState -> ServerState
doPhysics dt = objectsTransformed -- . playersTransformed 
  where
   -- playersTransformed ss = ss {players = map doPlayerPhysics (players ss)}
    objectsTransformed ss = ss {objects = map (doObjectPhysics dt) (objects ss)}
      
doObjectPhysics :: Double -> Object -> Object
doObjectPhysics dt obj = obj {pos = scale dt (vel obj) + pos obj}

intersection :: Object -> Object -> Bool 
intersection a b = c vecX && c vecY && c vecZ
  where
    -- Actual Wizardry
    c f = f at < f bo || f bt < f ao
    ao = pos a
    bo = pos b
    at = size a + pos a
    bt = size b + pos b

{-
 - Resolve all object collisions
 - Move each object to next position-}
 {-
  - No Intersect if:
  -   a max x < b min x or b max x < a min x
  -   and
  -   same for y/z
  -  -} 

module PurityPhysics where
import PurityData
import PurityUtil

import Debug.Trace

doPhysics :: Double -> ServerState -> ServerState
doPhysics dt ss = transformObjects (doObjectPhysics dt ss) . transformPlayers (doPlayerPhysics dt ss) $ ss 

-- Move an object, all said and done
doObjectPhysics :: Double -> ServerState -> Object -> Object
doObjectPhysics dt s = tickPosition dt . collideWithWorld . applyGravity dt . tickAcceleration dt
  where
    collideWithWorld = listApFold (map (\w x -> if intersectAABBWP (objAABB x) w then repairWPIntersection w x else x) . geometry . world $ s)

-- Do physics on a player; let objectphysics soak extra arguments
doPlayerPhysics :: Double -> ServerState -> Player -> Player
doPlayerPhysics = (transformObject .) . doObjectPhysics 

-- Move by velocity
tickPosition :: Double -> Object -> Object
tickPosition dt obj = obj {pos = scale dt (vel obj) + pos obj}

--TODO: FACTOR OUT MAGIC NUMBER
-- Apply gravity to velocity
applyGravity :: Double -> Object -> Object
applyGravity dt obj = obj {vel = vel obj - Vector (0,9 * dt,0)}

-- Apply internal acceleration to velocity
tickAcceleration :: Double -> Object -> Object
tickAcceleration dt obj = obj {vel = scale accelSpeed absWish + vel obj}
  where
    -- wishDir is relative to local coords, so we need to convert to abs coords to move (rotation only)
    absWish = wish obj
    -- Current speed, as dot product on wishDir
    currSpeed = dotProduct (normalizeVector absWish) (vel obj) 
    -- Max speed that we can add
    addSpeed = max (magnitude absWish - currSpeed) 0
    -- Actual speed to add
    --TODO: FACTOR OUT MAGIC NUMBER
    accelSpeed = min (12 * dt * magnitude absWish) addSpeed

repairWPIntersection :: WorldPlane -> Object -> Object
repairWPIntersection wp obj = traceShowId $ obj {vel = newvel}
  where
    dp = dotProduct (vel obj) nor -- 1 if away, -1 if towards, 0 if _|_ etc
    newvel = if dp > 0
      then vel obj
      else vel obj - scale (springCo * dp) nor -- questionable method
    nor = normalWP wp
    springCo = 1

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

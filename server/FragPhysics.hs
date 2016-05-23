module FragPhysics where
import FragData
import FragUtil
doPhysics :: Double -> ServerState -> ServerState
doPhysics dt ss = ss {
  objects = map (collideWithWorld . tickPosition . tickGravity . tickAcceleration) (objects ss),
  players = map (transformObject collideWithWorld . transformObject tickPosition . transformObject tickGravity . transformObject tickAcceleration) (players ss)}
  where
    collideWithWorld = listApFold (map (\w x -> if intersectAABBWP (objAABB x) w then repairWPIntersection w x else x) . geometry . world $ ss)
    -- Move all objects to next location, regardless of any collisions
    tickAcceleration = tickObjectAcceleration dt
    tickPosition = tickObjectPosition dt 
    tickGravity = applyGravity dt 
      
tickObjectPosition :: Double -> Object -> Object
tickObjectPosition dt obj = obj {pos = scale dt (vel obj) + pos obj}

--TODO: FACTOR OUT MAGIC NUMBER
applyGravity :: Double -> Object -> Object
applyGravity dt obj = obj {vel = vel obj - Vector (0,1 * dt,0)}

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
    --TODO: FACTOR OUT MAGIC NUMBER
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

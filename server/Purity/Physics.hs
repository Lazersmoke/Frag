module Purity.Physics where

import Purity.Data
import Purity.Util

import Data.Access

doPhysics :: PhysicsDescriptor -> ServerState -> ServerState
doPhysics pd ss = liftMap objects (doObjectPhysics pd ss) . liftMap players (doPlayerPhysics pd ss) $ ss 

-- Move an object, all said and done
doObjectPhysics :: PhysicsDescriptor -> ServerState -> Object -> Object
doObjectPhysics pd s = 
  tickPosition dt 
  . collideWithWorld 
  . applyFriction dt (frictionConst pd) 
  . applyGravity dt (gravityConst pd) 
  . tickAcceleration dt
  where
    dt = deltaTime pd
    collideWithWorld = listApFold (map (\w x -> if intersectAABBWP (objAABB x) w then repairWPIntersection w x else set mode InAir x) . geometry . grab world $ s)

-- Do physics on a player; let objectphysics soak extra arguments
doPlayerPhysics :: PhysicsDescriptor -> ServerState -> Player -> Player
doPlayerPhysics = (lift object .) . doObjectPhysics 

-- Move by velocity
tickPosition :: Double -> Object -> Object
tickPosition dt obj = pos >&> (+ scale dt (vel ~>> obj)) $ obj

-- Apply gravity to velocity
applyGravity :: Double -> Double -> Object -> Object
applyGravity dt grav = vel >&> subtract (Vector (0,grav * dt,0)) 

-- Apply friction to velocity
applyFriction :: Double -> Double -> Object -> Object
applyFriction dt fric obj = vel >&> subtract (scale (dt * fric) (vel ~>> obj)) $ obj

-- TODO: Rework groundedness
-- Apply internal acceleration to velocity
tickAcceleration :: Double -> Object -> Object
tickAcceleration dt obj = accelerate dt obj
  where
    accelerate = case mode ~>> obj of
      OnGround -> groundAccelerate 
      InAir -> airAccelerate

airAccelerate :: Double -> Object -> Object
airAccelerate dt o = (accelerateVector dt . set vecY 0 . scale 5 . rotObj o $ wish ~>> o) o

-- Accelerate on a vector
accelerateVector :: Double -> Vector -> Object -> Object
accelerateVector dt along obj = vel >&> (+ finalChange) $ obj
  where
    -- Current speed, as dot product on wishDir
    currSpeed = dotProduct (normalizeVector along) (vel ~>> obj) 
    -- Max speed that we can add
    addSpeed = max (magnitude along - currSpeed) 0
    -- Actual speed to add
    --TODO: FACTOR OUT MAGIC NUMBER
    accelSpeed = min (12 * dt * magnitude along) addSpeed
    finalChange = scale accelSpeed (normalizeVector along)

groundAccelerate :: Double -> Object -> Object
groundAccelerate dt obj = accelerateVector dt (scale 5 . (* Vector (1,15,1)) . rotatePitchYaw 0 (yaw ~>> obj) $ wish ~>> obj) obj

repairWPIntersection :: WorldPlane -> Object -> Object
repairWPIntersection wp obj = 
  if intersectAABBWP (objAABB obj) wp
    then (mode >@> OnGround) . (vel >@> newvel) $ obj
    else obj
  where
    dp = dotProduct (vel ~>> obj) nor -- 1 if away, -1 if towards, 0 if _|_ etc
    newvel = if dp > 0
      then vel ~>> obj
      else vel ~>> obj - scale (springCo * dp) nor -- questionable method
    nor = normalWP wp
    springCo = 1

intersectAABBWP :: AABB -> WorldPlane -> Bool
intersectAABBWP aabb wp@(WorldPlane (v1,v2,v3)) = 
  -- If there is no separating axis, then they must intersect
  not $ foundClearAABBNormal || foundClearWPNormal || foundClearCrossProduct
  where
    wpCorners = [v1,v2,v3]
    wpEdges = [v1-v3,v2-v1,v3-v2]
    wpNormal = normalWP wp

    -- Unit vectors are the normals to an AABB
    foundClearAABBNormal = any aabbNormalClear unitVectors
    -- True if the WP is fully off to one side of the box on the given axis
    aabbNormalClear axis = areSeparated (projectWP wp axis) (map (vecSum . (* axis)) wpCorners)

    -- True if the box is fully on one side of the WP's infinite plane
    foundClearWPNormal = areSeparated (projectAABB aabb wpNormal) [wpNormalOffset] 
    wpNormalOffset = dotProduct wpNormal v1

    -- True if any of the cross products between aabb normals and triangle edges are a separating plane
    foundClearCrossProduct = or [areSeparated (boxCross t b) (triCross t b) | t <- wpEdges, b <- unitVectors]

    -- Project the AABB onto the cross vector
    boxCross tri box = projectAABB aabb (tri `crossProduct` box)
    -- Project the WP onto the cross vector
    triCross tri box = projectWP wp (tri `crossProduct` box)

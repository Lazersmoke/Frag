module Purity.Server.Physics where

import Purity.Server.Data
import Purity.Server.Util

import Data.Access

doPhysics :: PhysicsDescriptor -> ServerState -> ServerState
doPhysics pd ss = liftMap ObjectA (doObjectPhysics pd ss) . liftMap Players (doPlayerPhysics pd ss) $ ss 

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
    collideWithWorld = listApFold (map (\w x -> if intersectAABBWP (objAABB x) w then repairWPIntersection w x else set Status InAir x) . grab Geometry . grab AccessWorld $ s)

-- Do physics on a player; let objectphysics soak extra arguments
doPlayerPhysics :: PhysicsDescriptor -> ServerState -> Player -> Player
doPlayerPhysics = (lift ObjectA .) . doObjectPhysics 

-- Move by velocity
tickPosition :: Double -> Object -> Object
tickPosition dt obj = Position >&> (+ scale dt (Velocity ~>> obj)) $ obj

-- Apply gravity to velocity
applyGravity :: Double -> Double -> Object -> Object
applyGravity dt grav = Velocity >&> subtract (Vector (0,grav * dt,0)) 

-- Apply friction to velocity
applyFriction :: Double -> Double -> Object -> Object
applyFriction dt fric obj = Velocity >&> subtract (scale (dt * fric) (Velocity ~>> obj)) $ obj

-- TODO: Rework groundedness
-- Apply internal acceleration to velocity
tickAcceleration :: Double -> Object -> Object
tickAcceleration dt obj = accelerate dt obj
  where
    accelerate = case Status ~>> obj of
      OnGround -> groundAccelerate 
      InAir -> airAccelerate

-- How to accelerate in air:
-- Find the wish direction, in relative coords
-- Rotate that to absolute coords
-- Remove vertical component
-- Accelerate along that vector
airAccelerate :: Double -> Object -> Object
airAccelerate dt o = accelerateVector dt (scale 5 . (Y >@> 0) . rotObj o $ Wish ~>> o) o

-- How to accelerate on the ground:
-- Find the wish direction (relative)
-- Rotate that only horizontally (not pitch)
-- Amplify by 1,15,1
-- Normalize and scale by 5 (set magnitude to 5)
-- Accelerate along that vector
groundAccelerate :: Double -> Object -> Object
groundAccelerate dt obj = accelerateVector dt (scale 5 . rotObj obj $ Wish ~>> obj) obj

-- Accelerate on a vector
accelerateVector :: Double -> Vector -> Object -> Object
accelerateVector dt along obj = Velocity >&> (+ finalChange) $ obj
  where
    -- Current speed, as dot product on wishDir
    currSpeed = dotProduct (normalizeVector along) (Velocity ~>> obj) 
    -- Max speed that we can add
    addSpeed = max (magnitude along - currSpeed) 0
    -- Actual speed to add
    --TODO: FACTOR OUT MAGIC NUMBER
    accelSpeed = min (12 * dt * magnitude along) addSpeed
    finalChange = scale accelSpeed (normalizeVector along)


repairWPIntersection :: WorldPlane -> Object -> Object
repairWPIntersection wp obj = 
  if intersectAABBWP (objAABB obj) wp
    then (if dotProduct nor (Vector (0,1,0)) > 0.1 then Status >@> OnGround else id) . (Velocity >@> newvel) $ obj
    else obj
  where
    dp = dotProduct (Velocity ~>> obj) nor -- 1 if away, -1 if towards, 0 if _|_ etc
    newvel = if dp > 0
      then Velocity ~>> obj
      else Velocity ~>> obj - scale (springCo * dp) nor -- questionable method
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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
module Purity.Physics where

import Linear hiding (trace)
import Control.Lens
--import Control.Monad
--import Data.Foldable
import Debug.Trace
import Data.Semigroup
import Purity.Data

kindaCloseToZero :: (Ord a,Floating a) => a -> Bool
kindaCloseToZero = (< 0.001) . abs

-- | An infinite plane in space which doesn't pass through the origin
newtype Surface q a = Surface
  {_surfaceNormal :: q a -- Surface is at and normal to tip of vector
  } deriving (Eq,Ord)
makeLenses ''Surface

-- | A surface useful for testing
groundSurface :: Num a => Surface V3 a
groundSurface = Surface (V3 0 1 0)

-- | A physical subset of q a describing the shape of our object
data PhysDomain q a
  = AABBDomain (q a) deriving (Eq,Show)
  -- | SphereDomain a
  -- Origin is lower edge
  -- Height then radius
  -- CapsuleDomain a a

-- | The volume enclosed in a given PhysDomain
domainVolume :: (Floating a, Metric q, Foldable q) => PhysDomain q a -> a
domainVolume (AABBDomain size) = product size
--domainVolume (SphereDomain r) = 4/3 * pi * r * r * r
--domainVolume (CapsuleDomain h r) = (4/3 * r + h) * pi * r * r

-- | The center of a given PhysDomain
domainCenter :: (Floating a, Additive q) => PhysDomain q a -> q a
domainCenter (AABBDomain size) = 0.5 *^ size

-- | A description of an object's velocity, position, orientation, and shape
data PhysModel q a = PhysModel
  {_physDomain :: PhysDomain q a
  ,_currentVelocity :: q a
  ,_currentOrigin :: q a
  ,_currentOrientation :: Quaternion a
  } deriving Eq
makeLenses ''PhysModel

instance (Show a, Show (q a)) => Show (PhysModel q a) where
  show model = blue "[" ++ green ("At: " ++ show (model^.currentOrigin)) ++ blue " | " ++ green ("Vel: " ++ show (model^.currentVelocity)) ++ blue " | " ++ "Look: " ++ show (model^.currentOrientation) ++ blue " | " ++ "Dom: " ++ show (model^.physDomain) ++ blue "]"

-- | Where the object is now, plus the applied force.
-- We can use this to meaningfully sample where it will be in the future, thus the name.
data PhysFuture q a = PhysFuture
  {_presentModel :: PhysModel q a
  ,_appliedForce :: q a
  } deriving Eq
makeLenses ''PhysFuture

-- | We blinked and saw the object at a specific time, and now we can figure out where
-- it will be in the future.
data PhysBlink q a = PhysBlink
  {_blinkVision :: PhysFuture q a
  ,_blinkTime :: a
  } deriving Eq
makeLenses ''PhysBlink

-- | Construct a blink given the time and what we saw
blinkAt :: a -> PhysFuture q a -> PhysBlink q a
blinkAt = flip PhysBlink

instance (Eq (q a),Ord a) => Ord (PhysBlink q a) where
  a <= b = a^.blinkTime <= b^.blinkTime

instance (Show a, Show (q a)) => Show (PhysBlink q a) where
  show b = "BLINK[" ++ show (b^.blinkTime) ++ "]" ++ show (b^.blinkVision)

-- | We blinked and saw the object, then decided when we will need to update its path
data PhysGlide q a = PhysGlide
  {_exigentBlink :: PhysBlink q a
  ,_plannedBlink :: Maybe (PhysBlink q a)
  }
makeLenses ''PhysGlide

instance (Show a, Show (q a)) => Show (PhysGlide q a) where
  show f = cyan "Exigent: " ++ show (f^.exigentBlink) ++ cyan " Planned: " ++ show (f^.plannedBlink)

-- | We blinked and saw that the object will never collide with anything forevermore
glideForever :: PhysBlink q a -> PhysGlide q a
glideForever b = PhysGlide b Nothing

-- | Apply exactly one force to a PhysModel so we can sample its future
applyForce :: q a -> PhysModel q a -> PhysFuture q a
applyForce f m = PhysFuture {_presentModel = m, _appliedForce = f}

instance (Show a, Show (q a)) => Show (PhysFuture q a) where
  show f = green "{" ++ green ("Apply " ++ show (f^.appliedForce)) ++ " to " ++ show (f^.presentModel) ++ green "}"

-- | Seek forward a PhysFuture by a specified delta time, returning an updated PhysFuture
seekFuture :: (Additive q, Fractional a) => a -> PhysFuture q a -> PhysFuture q a
seekFuture t f = presentModel .~ readFuture t f $ f

--translateDomain :: (Num a, Additive q) => q a -> PhysDomain q a -> PhysDomain q a
--translateDomain off (AABB low high) = AABB (low ^+^ off) (high ^+^ off)

translationConstantAccel :: (Fractional a, Additive q) => a -> q a -> q a -> q a
translationConstantAccel dt v a = dt *^ v ^+^ 0.5 * dt * dt *^ a

velocityConstantAccel :: (Fractional a, Additive q) => a -> q a -> q a -> q a
velocityConstantAccel dt v a = v ^+^ dt *^ a

-- | Sample forward the PhysFuture to find the PhysModel after a given amount of time
readFuture :: (Fractional a, Additive q) => a -> PhysFuture q a -> PhysModel q a
readFuture dt f = updateVel . updatePos $ f^.presentModel
  where
    updateVel = currentVelocity %~ (^+^ dt *^ f^.appliedForce)
    updatePos = currentOrigin %~ (^+^ translationConstantAccel dt (f^.presentModel.currentVelocity) (f^.appliedForce))

-- | Extrapolate a blink forward by dt
reblink :: (Additive q,Fractional a,Num a) => a -> PhysBlink q a -> PhysBlink q a
reblink dt = (blinkVision %~ seekFuture dt) . (blinkTime +~ dt)

-- | Extrapolate a blink to an absolute time
blinkTo :: (Additive q,Fractional a,Num a) => a -> PhysBlink q a -> PhysBlink q a
blinkTo t b = (blinkVision %~ seekFuture (t - (b^.blinkTime))) . (blinkTime .~ t) $ b

-- | Sample a blink from a glide at an absolute time
glideTo :: (Additive q,Fractional a,Ord a,Num a) => a -> PhysGlide q a -> PhysBlink q a
glideTo t g = case g^.plannedBlink of
  Just plBl | t >= plBl^.blinkTime -> blinkTo t plBl
  _ -> blinkTo t (g^.exigentBlink)

-- | Returns the first time the two simultaneous futures intersect after they start
-- This operation is symmetric (anti-symmetry is a bug)
timeOfIntersection :: (Foldable q, Applicative q, Show a, Ord a, Floating a, Metric q) => PhysFuture q a -> PhysFuture q a -> Maybe a
timeOfIntersection fa fb = foldl maybeMin Nothing . rectifyQuads $ eqsOfMotion fa fb

-- | Gets the minimum of two maybe values, preferring to take any value it finds over throwing anything out
maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMin (Just a) (Just b) = Just $ min a b
maybeMin (Just a) _ = Just a
maybeMin _ x = x

-- | Returns the first (wrt absolute time) intersection of the two blinks, in the form of the first blink forwarded to immediately after the intersection
blinkIntersect :: (Epsilon a,Foldable q, Applicative q, Show (q [a]), Show a, Ord a, Floating a, Metric q) => PhysBlink q a -> PhysBlink q a -> Maybe (PhysBlink q a)
blinkIntersect ba bb = fixup . (baseTime +) <$> timeOfIntersection fa fb
  where
    (fa,fb) = if (ba^.blinkTime) > (bb^.blinkTime)
      then (ba^.blinkVision, seekFuture blinkDrift (bb^.blinkVision))
      else (seekFuture blinkDrift (ba^.blinkVision), bb^.blinkVision)
    blinkDrift = abs $ (ba^.blinkTime) - (bb^.blinkTime)
    baseTime = max (ba^.blinkTime) (bb^.blinkTime)
    fixup t = blinkVision.presentModel .~ resultCollisionModel (blinkTo t bb^.blinkVision.presentModel) (blinkTo t ba^.blinkVision.presentModel) $ blinkTo t ba


-- | Returns the dim q many quadratics with bounds representing collisions on each axis of the two PhysFutures
eqsOfMotion :: (Foldable q, Applicative q, Show a, Ord a, Floating a, Metric q) => PhysFuture q a -> PhysFuture q a -> q (Quad a,V2 a)
eqsOfMotion fa fb = do
  aA <- fa^.appliedForce
  aB <- fb^.appliedForce
  vA <- fa^.presentModel.currentVelocity
  vB <- fb^.presentModel.currentVelocity
  xA <- fa^.presentModel.currentOrigin
  xB <- fb^.presentModel.currentOrigin
  qA' <- qA
  qB' <- qB
  pure (V3 (xB - xA) (vB - vA) ((aB - aA)/2),V2 (-qB') qA')
  where
    (AABBDomain qA) = fa^.presentModel.physDomain
    (AABBDomain qB) = fb^.presentModel.physDomain

data Sign = Plus | Minus | Zero deriving (Eq,Show)

signOf :: (Ord a, Num a) => a -> Sign
signOf x = if x > 0 then Plus else (if x < 0 then Minus else Zero)

oppSign :: Sign -> Sign
oppSign Plus = Minus
oppSign Minus = Plus
oppSign Zero = Zero

-- | Lists the roots of odd multiplicity of the given Quad
-- Even roots don't matter because they "bounce off the axis," so can never be a transition between valid and invalid (negative and non-negative) states.
quadOddRoots :: (Ord a, Floating a) => Quad a -> [a]
quadOddRoots (V3 c b a) = case signOf det of
  Plus -> case signOf a of
    Plus -> [(-b - sqrt det)/(2 * a),(-b + sqrt det)/(2 * a)]
    Minus -> [(-b + sqrt det)/(2 * a),(-b - sqrt det)/(2 * a)]
    Zero -> linOddRoots c b -- Was actually linear
  _ -> [] -- Either no roots, or only an even one
  where
    det = b * b - 4 * a * c

-- | bx + c = 0 ==> x = -c/b
linOddRoots :: (Eq a, Fractional a) => a -> a -> [a]
linOddRoots c b = if b == 0 then [] else [-c/b]

-- | Evaluate a Quad at an input value
evalQuad :: Num a => a -> Quad a -> a
evalQuad x (V3 c b a) = a * x * x + b * x + c

-- | Evaluate a Quad's derivative at an input value
evalQuad' :: Num a => a -> Quad a -> a
evalQuad' x (V3 _ b a) = 2 * a * x + b

-- | Takes a vector of quadratic equations with a single codomain bounding interval for each, and returns the first time (if any) for which *all* components satisfy their bounds
rectifyQuads :: (Functor q, Foldable q, Ord a, Floating a, Show a) => q (Quad a, V2 a) -> q (Maybe a)
rectifyQuads qs = fmap (safeHead . dropWhile (not . condition)) roots
  where
    -- The condition is that all components simultaneously satisfy the bounds
    condition x = getAll . foldMap (All . satisfiesBounds x) $ qs
    -- An input satifies the bounds of the Quad if:
    satisfiesBounds x (q,V2 low high) = trace ("Bounds check: x=" ++ show x ++ " in " ++ show q ++ " satisfying bounds " ++ show (V2 low high)) $ let 
      e = evalQuad x q 
      d = evalQuad' x q 
      nominallyLTE a b = a <= b || kindaCloseToZero (a - b)
      in 
        -- Not intersecting only nominally and fixing itself
        not (kindaCloseToZero (e - high) && d > 0)
        && not (kindaCloseToZero (e - low) && d < 0)
        -- and also nominally intersecting in general
        && nominallyLTE e high
        && nominallyLTE low e
    -- zqs are the equations (for each component) which have zeros of odd multiplicity
    -- when the given Quads transition between satisfying and 
    -- not satisfying the boundary conditions
    -- q [Quad q a]
    zqs = fmap traceShowId . fmap (\(q,V2 low high) -> traceShow (q,V2 low high) $ [q ^-^ constantQuad low,q ^-^ constantQuad high]) $ qs
    -- roots is the collection (for each component) of all such odd zeros
    -- that are in the future (dt > 0)
    -- q [a]
    roots = fmap (dropWhile (< 0) . foldr merge [] . fmap quadOddRoots) $ zqs

-- | Safely take the first element of a possibly empty list, returning Nothing for []
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Merge two sorted lists. Standard in merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | (x <= y)  = x:(merge xs (y:ys)) 
  | otherwise = y:(merge (x:xs) ys)

-- | V3 c b a := ax^2 + bx + c
type Quad = V3

-- | Makes a Quad representing the Quad which is constantly the input
constantQuad :: Num a => a -> Quad a
constantQuad x = V3 x 0 0

-- | Given a position vector and a PhysDomain, construct a default PhysModel which isn't moving and has default orientation
domainAtRest :: (Epsilon a,Additive q, Floating a) => q a -> PhysDomain q a -> PhysModel q a
domainAtRest x0 dom = PhysModel
  {_physDomain = dom
  ,_currentOrigin = x0
  ,_currentVelocity = zero
  ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
  }

-- | Apply gravity to a PhysModel to give it a future
gravityFuture :: PhysModel V3 Float -> PhysFuture V3 Float
gravityFuture = applyForce (V3 0 (-9.8) 0)

-- | A PhysFuture useful for testing
objAtZero :: PhysFuture V3 Float
objAtZero = PhysFuture
  {_appliedForce = V3 0 0.2 0
  ,_presentModel = PhysModel
    {_physDomain = AABBDomain (V3 2 1 2)
    ,_currentOrigin = V3 0 (-1) 0
    ,_currentVelocity = zero
    ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
    }
  }

-- | A PhysFuture useful for testing
objFalling :: PhysFuture V3 Float
objFalling = PhysFuture
  {_appliedForce = V3 0 (-1) 0
  ,_presentModel = PhysModel
    {_physDomain = AABBDomain (V3 1 1 1)
    ,_currentOrigin = V3 0 20 0
    ,_currentVelocity = zero
    ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
    }
  }

-- | A PhysFuture useful for testing
objLeft :: PhysFuture V3 Float
objLeft = PhysFuture
  {_appliedForce = V3 1 0 0
  ,_presentModel = PhysModel
    {_physDomain = AABBDomain (V3 2 2 2)
    ,_currentOrigin = V3 (-5) (-5) 0
    ,_currentVelocity = V3 (-2) 5 0
    ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
    }
  }

-- | A PhysFuture useful for testing
objRight :: PhysFuture V3 Float
objRight = PhysFuture
  {_appliedForce = V3 (-1) 0 0
  ,_presentModel = PhysModel
    {_physDomain = AABBDomain (V3 2 2 2)
    ,_currentOrigin = V3 5 (-5) 0
    ,_currentVelocity = V3 2 5 0
    ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
    }
  }

-- | Given two PhysModels which are assumed to be touching, give the second one after the collision occurs
resultCollisionModel :: (Epsilon a,Ord a,Floating a, Metric q) => PhysModel q a -> PhysModel q a -> PhysModel q a
resultCollisionModel col move = (currentVelocity %~ (^+^ normalResponse move col)) $ move

-- | Given two PhysModels which are assumed to be touching, give the change in velocity for the second one
normalResponse :: (Epsilon a,Ord a,Floating a, Metric q) => PhysModel q a -> PhysModel q a -> q a
normalResponse a b = normalize (collisionNormal b a) ^* (norm dv * collisionElasticity)
  where
    dv = a^.currentVelocity ^-^ b^.currentVelocity
    collisionElasticity = 1.01

-- Points from B to A (on centers)
-- | Given two PhysModels which are assumed to be touching, give the collision normal vector.
-- Norm should be unit, but YMMV (normalize)
collisionNormal :: (Ord a,Floating a,Additive q) => PhysModel q a -> PhysModel q a -> q a
collisionNormal a b = topA ^+^ topB --(a^.currentOrigin ^+^ a^.physDomain.to domainCenter) ^-^ (b^.currentOrigin ^+^ b^.physDomain.to domainCenter)
  where
    topA = fmap (\x -> if kindaCloseToZero x then 1 else 0) $ (a^.currentOrigin ^+^ qA) ^-^ (b^.currentOrigin)
    topB = fmap (\x -> if kindaCloseToZero x then (-1) else 0) $ (a^.currentOrigin) ^-^ (b^.currentOrigin ^+^ qB)
    (AABBDomain qA) = a^.physDomain
    (AABBDomain qB) = b^.physDomain

-- | Given the rest of the blinks to consider, tell how a single blink will next collide
nextBlink :: (Epsilon a,Show (q [a]),Eq (q a),Foldable q, Applicative q, Show a, Ord a, Floating a, Metric q) => [PhysBlink q a] -> PhysBlink q a -> Maybe (PhysBlink q a)
nextBlink bs b = fmap getMin . getOption . foldMap (Option . fmap Min) $ blinkIntersect b <$> (filter (/= b) bs)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Purity.Physics where

import Linear
import Control.Lens
import qualified Data.Set as Set

-- A point mass in space. How cute.
data FreeBody q a = FreeBody
  {_fbPosition :: q a
  ,_fbVelocity :: q a
  }
makeLenses ''FreeBody

data Surface q a = Surface
  {_surfaceNormal :: q a -- Unit
  ,_surfaceOffset :: a -- normals to get to surface from origin. surfaceNormal * surfaceOffset is on the surface
  } deriving (Eq,Ord)
makeLenses ''Surface

-- A point mass in space. How cute.
data PhysicsBody q a = PhysicsBody
  {_pbFreeBody :: FreeBody q a
  ,_pbContacts :: Set.Set (Surface q a)
  }
makeLenses ''PhysicsBody

instance (Additive q,R3 q,Num a,Show a) => Show (FreeBody q a) where
  show fb = "{[o](" 
    ++ show (fb^.fbPosition^._x) ++ ","
    ++ show (fb^.fbPosition^._y) ++ ","
    ++ show (fb^.fbPosition^._z) ++ ")|[->]("
    ++ show (fb^.fbVelocity^._x) ++ ","
    ++ show (fb^.fbVelocity^._y) ++ ","
    ++ show (fb^.fbVelocity^._z) ++ ")}"

bodyAtRest :: (Additive q,Num a) => q a -> FreeBody q a
bodyAtRest pos = FreeBody {_fbPosition = pos,_fbVelocity = zero}

surfaceOriginPoint :: (Functor q,Num a) => Surface q a -> q a
surfaceOriginPoint s = s^.surfaceNormal ^* s^.surfaceOffset

prioriDump :: (Metric q, Ord a, Floating a) => q a -> Surface q a -> FreeBody q a -> (a,a,a,a,Bool,a,a)
prioriDump f s fb = (spfNormal,fbvNormal,disNormal,determinant,willCollide,solnA,solnB)
  where
    disNormal = distanceToSurface s (fb^.fbPosition)
    fbvNormal = alongSurface s (fb^.fbVelocity)
    spfNormal = alongSurface s f
    determinant = fbvNormal * fbvNormal - 2 * spfNormal * disNormal
    willCollide = determinant >= 0
    -- Solve: 0.5at^2 + vt = d
    solnA = (negate fbvNormal + sqrt determinant) / spfNormal
    solnB = (negate fbvNormal - sqrt determinant) / spfNormal

timeOfImpact :: (Metric q, Ord a, Floating a) => q a -> Surface q a -> FreeBody q a -> Maybe a
timeOfImpact f s fb = if willCollide then Just (minimum solns) else Nothing
  where
    disNormal = distanceToSurface s (fb^.fbPosition)
    fbvNormal = alongSurface s (fb^.fbVelocity)
    spfNormal = alongSurface s f
    determinant = fbvNormal * fbvNormal - 2 * spfNormal * disNormal
    willCollide = determinant >= 0 && (not . null) solns
    -- Solve: 0.5at^2 + vt = d
    -- Take only solutions in the future
    solns = filter (>=0) $ map (\pm -> (negate fbvNormal + pm (sqrt determinant)) / spfNormal) [negate,id]

constrained :: PhysicsBody q a -> FreeBody q a
constrained 

sampleForward :: (Metric q, Ord a, Floating a) => a -> q a -> Surface q a -> PhysicsBody q a -> PhysicsBody q a
sampleForward dt f s pb = case timeOfImpact f s pb of
  Just tImpact -> if tImpact <= dt
    then (pbContacts %~ Set.insert s) . (pbFreeBody %~ simulateFreeForce tImpact f) $ pb
    else pbFreeBody %~ simulateFreeForce dt f $ pb
  Nothing -> pbFreeBody %~ simulateFreeForce dt f $ pb

simulateConstrainedForce :: (Metric q,Num a,Ord a) => a -> q a -> PhysicsBody q a -> PhysicsBody q a
simulateConstrainedForce dt f pb = (pbFreeBody %~ simulateFreeForce dt (fullCancel f)) . (pbFreeBody.fbVelocity %~ fullCancel) $ pb
  where
    fullCancel x = foldr normalCancel x (pb^.pbContacts)

sampleForceCollide :: (Metric q, Ord a, Floating a) => q a -> Surface q a -> a -> FreeBody q a -> FreeBody q a
sampleForceCollide f s t fb = case timeOfImpact f s fb of
  Just tCol | tCol < t -> sampleForwardForce (normalCancel s f) (t - tCol) $ fbVelocity %~ normalCancel s $ sampleForwardForce f tCol fb
  _ -> sampleForwardForce f t fb

-- Apply an acceleration for an amount of time and simulate the resulting free motion
sampleForwardForce :: (Additive q, Fractional a) => q a -> a -> FreeBody q a -> FreeBody q a
sampleForwardForce f t fb = (fbPosition %~ (^+^) ((fb^.fbVelocity ^* t) ^+^ 0.5 *^ f ^* (t * t))) . (fbVelocity %~ (^+^) (t *^ f)) $ fb

alongSurface :: (Metric q, Num a) => Surface q a -> q a -> a
alongSurface s q = dot q (s^.surfaceNormal)

-- Positive when above surface, negative when below
distanceToSurface :: (Metric q,Num a) => Surface q a -> q a -> a
distanceToSurface s floating = alongSurface s floating - s^.surfaceOffset

applyForce :: (Additive q,Num a) => a -> q a -> FreeBody q a -> FreeBody q a
applyForce dt f = fbVelocity %~ (^+^) (f ^* dt)

gravity :: Fractional a => V3 a
gravity = V3 0 (-9.8) 0

sideKick :: Num a => V3 a
sideKick = V3 1 0 0

normalReaction :: (Metric q,Num a) => q a -> Surface q a -> q a
normalReaction exigentForce surface = surface^.surfaceNormal ^* alongSurface surface (negated exigentForce)

normalCancel :: (Metric q,Num a,Ord a) => Surface q a -> q a -> q a
normalCancel s f = f ^-^ (s^.surfaceNormal ^* min 0 (alongSurface s f))

groundSurface :: Num a => Surface V3 a
groundSurface = surfaceThroughOrigin (V3 0 1 0)

testBody :: (Fractional a,Num a) => FreeBody V3 a
testBody = applyForce 0.01 gravity $ applyForce 0.25 sideKick $ bodyAtRest (V3 0 1 0)

testBody' :: (Fractional a,Num a) => FreeBody V3 a
testBody' = applyForce 1 (negated gravity) $ bodyAtRest (V3 0 1 0)

surfaceThroughOrigin :: Num a => q a -> Surface q a
surfaceThroughOrigin n = Surface{_surfaceNormal = n,_surfaceOffset = 0}

checkStepWithSurface :: (Metric q,Ord a,Num a) => a -> FreeBody q a -> Surface q a -> Bool
checkStepWithSurface dt fb s = distanceToSurface s (fb^.fbPosition ^+^ step) > 0
  where
    step = dt *^ fb^.fbVelocity

-- Time until the collision
timeUntilCollision :: (Metric q,Ord a,Fractional a) => FreeBody q a -> Surface q a -> Maybe a
timeUntilCollision fb s = if velInDir == 0 || tCol < 0 then Nothing else Just tCol
  where
    tCol = distanceToSurface s (fb^.fbPosition) / velInDir
    -- Amount of velocity directed toward the top of the surface
    velInDir = negate $ alongSurface s (fb^.fbVelocity)

simulateStep :: (Metric q,Fractional a,Ord a) => a -> FreeBody q a -> Surface q a -> FreeBody q a
simulateStep dt fb s = case timeUntilCollision fb s of
  Just tCol | tCol <= dt -> simulateFreeStep (dt - tCol) $ fbVelocity %~ normalCancel s $ atCol
    where
      atCol = simulateFreeStep tCol fb
  _ -> simulateFreeStep dt fb

simulateFreeStep :: (Additive q,Num a) => a -> FreeBody q a -> FreeBody q a
simulateFreeStep dt fb = fbPosition %~ (^+^) (dt *^ fb^.fbVelocity) $ fb

simulateFreeForce :: (Additive q,Num a) => a -> q a -> FreeBody q a -> FreeBody q a
simulateFreeForce dt f fb = (fbVelocity %~ (^+^) (dt *^ f)) . (fbPosition %~ (^+^) (dt *^ fb^.fbVelocity)) $ fb

simulateForce :: (Metric q,Fractional a,Num a,Ord a) => a -> q a -> Surface q a -> FreeBody q a -> FreeBody q a
simulateForce dt f s fb = case timeUntilCollision fb s of 
  Just tCol | tCol <= dt -> simulateFreeForce (dt - tCol) f $ fbVelocity %~ normalCancel s $ atCol
    where
      atCol = simulateFreeForce tCol f fb
  _ -> simulateFreeForce dt f fb

{-
freeMotion :: (Additive q,Num a) => FreeBody q a -> Motion q a
freeMotion fb = motionFromSampleFunction $ \dt -> fbPosition %~ (^+^) (dt *^ fb^.fbVelocity) $ fb

motionUntilSurface :: (Metric q,Fractional a,Ord a) => FreeBody q a -> Surface q a -> Motion q a
motionUntilSurface fb s = case timeUntilCollision fb s of
  -- Collides some time, so switch to free motion after cancellation
  Just tCol -> consMotion tCol (freeMotion fb) (freeMotion atCol)
    where
      atCol = fbVelocity %~ normalCancel s $ (freeMotion fb)^.sampleMotion tCol
  -- Never collides, so free motion
  Nothing -> freeMotion fb

noMotion :: FreeBody q a -> Motion q a
noMotion = motionFromSampleFunction . const
-}

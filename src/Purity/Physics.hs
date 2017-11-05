{-# LANGUAGE TemplateHaskell #-}
module Purity.Physics where

import Linear
import Control.Lens

data FreeBody a = FreeBody
  {_freeBodyPosition :: V3 a
  ,_freeBodyVelocity :: V3 a
  }

makeLenses ''FreeBody

instance Show a => Show (FreeBody a) where
  show fb = "{[o](" 
    ++ show (fb^.freeBodyPosition^._x) ++ ","
    ++ show (fb^.freeBodyPosition^._y) ++ ","
    ++ show (fb^.freeBodyPosition^._z) ++ ")|[->]("
    ++ show (fb^.freeBodyVelocity^._x) ++ ","
    ++ show (fb^.freeBodyVelocity^._y) ++ ","
    ++ show (fb^.freeBodyVelocity^._z) ++ ")}"

bodyAtRest :: Num a => V3 a -> FreeBody a
bodyAtRest pos = FreeBody {_freeBodyPosition = pos,_freeBodyVelocity = V3 0 0 0}

data Surface a = Surface
  {_surfaceNormal :: V3 a -- Unit
  ,_surfaceOffset :: a -- Distance to origin along normal
  }

makeLenses ''Surface

surfaceOriginPoint :: Num a => Surface a -> V3 a
surfaceOriginPoint s = s^.surfaceNormal ^* s^.surfaceOffset

distanceToSurface :: Num a => V3 a -> Surface a -> a
distanceToSurface floating s = s^.surfaceOffset - dot floating (s^.surfaceNormal)

applyForce :: Num a => V3 a -> a -> FreeBody a -> FreeBody a
applyForce f dt = freeBodyVelocity +~ f ^* dt

gravity :: Fractional a => V3 a
gravity = V3 0 (-9.8) 0

sideKick :: Num a => V3 a
sideKick = V3 1 0 0

normalForce :: Num a => V3 a -> Surface a -> V3 a
normalForce exigentForce surface = surface^.surfaceNormal ^* dot (surface^.surfaceNormal) exigentForce

groundSurface :: Num a => Surface a
groundSurface = surfaceThroughOrigin (V3 0 1 0)

testBody :: (Fractional a,Num a) => FreeBody a
testBody = applyForce gravity 0.01 $ applyForce sideKick 0.25 $ bodyAtRest (V3 0 1 0)

surfaceThroughOrigin :: Num a => V3 a -> Surface a
surfaceThroughOrigin n = Surface{_surfaceNormal = n,_surfaceOffset = 0}

checkStepWithSurface :: (Ord a, Num a) => a -> FreeBody a -> Surface a -> Bool
checkStepWithSurface dt fb s = distanceToSurface (fb^.freeBodyPosition + step) s < 0
  where
    step = dt *^ fb^.freeBodyVelocity

timeOfCollision :: (Ord a,Fractional a,Num a) => FreeBody a -> Surface a -> Maybe a
timeOfCollision fb s = if velInDir == 0 || tCol < 0 then Nothing else Just tCol
  where
    tCol = distanceToSurface (fb^.freeBodyPosition) s / velInDir
    velInDir = dot (fb^.freeBodyVelocity) (s^.surfaceNormal)

simulateStep :: (Fractional a,Num a,Ord a) => a -> FreeBody a -> Surface a -> FreeBody a
simulateStep dt fb s = case timeOfCollision fb s of
  Just tCol | tCol <= dt -> simulateFreeStep (dt - tCol) $ freeBodyVelocity -~ s^.surfaceNormal ^* dot (atCol^.freeBodyVelocity) (s^.surfaceNormal) $ atCol
    where
      atCol = simulateFreeStep tCol fb
  _ -> simulateFreeStep dt fb

simulateFreeStep :: Num a => a -> FreeBody a -> FreeBody a
simulateFreeStep dt fb = freeBodyPosition +~ dt *^ fb^.freeBodyVelocity $ fb

simulateFreeForce :: Num a => a -> V3 a -> FreeBody a -> FreeBody a
simulateFreeForce dt f fb = (freeBodyVelocity +~ dt *^ f) . (freeBodyPosition +~ dt *^ fb^.freeBodyVelocity) $ fb

simulateForce :: (Fractional a,Num a,Ord a) => a -> V3 a -> Surface a -> FreeBody a -> FreeBody a
simulateForce dt f s fb = case timeOfCollision fb s of 
  Just tCol | tCol <= dt -> simulateFreeForce (dt - tCol) f $ freeBodyVelocity -~ s^.surfaceNormal ^* dot (atCol^.freeBodyVelocity) (s^.surfaceNormal) $ atCol
    where
      atCol = simulateFreeForce tCol f fb
  _ -> simulateFreeForce dt f fb

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Purity.Physics where

import Linear
import Control.Lens

newtype Surface q a = Surface
  {_surfaceNormal :: q a -- Surface is at and normal to tip of vector
  } deriving (Eq,Ord)
makeLenses ''Surface

groundSurface :: Num a => Surface V3 a
groundSurface = Surface (V3 0 1 0)

data PhysDomain q a = AABB (q a) (q a)

data PhysModel q a = PhysModel
  {_physDomain :: PhysDomain q a
  ,_currentVelocity :: q a
  ,_currentOrigin :: q a
  ,_currentOrientation :: Quaternion a
  }
makeLenses ''PhysModel

--translateDomain :: (Num a, Additive q) => q a -> PhysDomain q a -> PhysDomain q a
--translateDomain off (AABB low high) = AABB (low ^+^ off) (high ^+^ off)

translationConstantAccel :: (Fractional a, Additive q) => a -> q a -> q a -> q a
translationConstantAccel dt v a = dt *^ v ^+^ 0.5 * dt * dt *^ a

sampleModelForward :: (Fractional a, Additive q) => a -> q a -> PhysModel q a -> PhysModel q a
sampleModelForward dt a m = updateVel . updatePos $ m
  where
    updateVel = currentVelocity %~ (^+^ dt *^ a)
    updatePos = currentOrigin %~ (^+^ translationConstantAccel dt (m^.currentVelocity) a)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Purity.Physics where

import Linear
import Control.Lens
--import qualified Data.Map.Strict as Map

-- A point mass in space. How cute.
data FreeBody q a = FreeBody
  {_fbPosition :: q a
  ,_fbVelocity :: q a
  }
makeLenses ''FreeBody

-- An instantaneous change in acceleration
newtype Instant q a = Instant
  {_instantDelta :: q a
  } deriving (Eq,Ord)
makeLenses ''Instant

instance Show (q a) => Show (Instant q a) where
  show i = "Delta{" ++ i^.instantDelta.to show ++ "}"

-- A segment of time during which there is a constant acceleration
data Span t q a = Span
  {_spanDuration :: t
  ,_spanForce :: q a
  } deriving (Eq,Ord)
makeLenses ''Span

instance (Eq t, Num t, Show t, Show (q a)) => Show (Span t q a) where
  show sp 
    | sp^.spanDuration == 0 = "\x1b[31mWARNING: ZERO LENGTH SPAN with spanForce: " ++ sp^.spanDuration.to show ++ "\x1b[0m"
    | otherwise = "{" ++ sp^.spanForce.to show ++ "} for " ++ sp^.spanDuration.to show ++ "s"

spanDurationForce :: t -> q a -> Span t q a
spanDurationForce d f = Span
  {_spanDuration = d
  ,_spanForce = f
  }

data Surface q a = Surface
  {_surfaceNormal :: q a -- Unit
  ,_surfaceOffset :: a -- normals to get to surface from origin. surfaceNormal * surfaceOffset is on the surface
  }
makeLenses ''Surface

-- A model of an object's future motion, with enough information to rebuild that future based on new forces.
-- A motion is affine (has no initial position). Positions are relative to wherever the object starts.
data Motion t q a = Motion
  {_motionDuration :: t
  ,_motionForce :: q a
  ,_nextMotion :: Maybe (Motion t q a)
  } deriving Show
makeLenses ''Motion

-- Generalizes for example velocity dependent forces and normal accelerations
data IntegrationRule t q a = IntegrationRule
  {_sampleForward :: q a -> t -> FreeBody q a -> FreeBody q a
  ,_prioriImpact :: q a -> Surface q a -> FreeBody q a -> Maybe t
  }
makeLenses ''IntegrationRule

-- Add the first motion into the second motion where the motions start at the same time
mixInMotion :: (Additive q,Num a,Ord t,Num t) => Motion t q a -> Motion t q a -> Motion t q a
mixInMotion m' m = if
  -- If the mix-in goes past the first leg of the target, mix the first leg of the target, then mix with the rest of the target
  | m'^.motionDuration > m^.motionDuration -> (nextMotion %~ zipMix (Just $ motionDuration -~ m^.motionDuration $ m')) . (motionForce %~ (^+^ m'^.motionForce)) $ m
  -- If the mix-in just matches the first leg of the target, mix together directly
  -- (m') [m+m'] | mixInMotion etc{m'} etc{m}
  | m'^.motionDuration == m^.motionDuration -> (nextMotion %~ zipMix (m'^.nextMotion)) . (motionForce %~ (^+^ m'^.motionForce)) $ m
  -- If the mix-in stops before the end of the first leg of the target, mix the other way around
  | m'^.motionDuration < m^.motionDuration ->  mixInMotion m m'
  | otherwise -> error "Broken Ord instance for time in mixInMotion"
  where
    zipMix Nothing (Just a) = Just a
    zipMix (Just a) Nothing = Just a
    zipMix Nothing Nothing = Nothing
    zipMix (Just a) (Just b) = Just $ mixInMotion a b

motionDurationForce :: t -> q a -> Maybe (Motion t q a) -> Motion t q a
motionDurationForce = Motion

sampleMotion :: t -> Motion t q a -> FreeBody q a

{-
mixInForce :: (Additive q,Num a,Ord t,Num t) => t -> Span t q a -> Motion t q a -> Motion t q a
mixInForce t' f' = motionSpans %~ go t' f'
  where
    go t f (sp:sps) = if 
      -- The inserted span is inserted after the end of the first span
      | t >= sp^.spanDuration -> sp : go (t - sp^.spanDuration) f sps
      -- The inserted span is inserted during the current span
      | otherwise -> if
        -- The inserted span's duration exceeds that of the current span; modify the existing span and recurse with t=0
        | f^.spanDuration > sp^.spanDuration -> (spanForce %~ (^+^ f^.spanForce) $ sp) : go 0 (spanDuration %~ (subtract $ sp^.spanDuration) $ f) sps
        -- The inserted span's duration matches that of the current span; modify the existing span only
        | f^.spanDuration == sp^.spanDuration -> (spanForce %~ (^+^ f^.spanForce) $ sp) : sps
        -- The inserted span's duration is less than that of the current span; split into two spans
        | f^.spanDuration < sp^.spanDuration -> (spanForce %~ (^+^ sp^.spanForce) $ f) : (spanDuration %~ (subtract $ f^.spanDuration) $ sp) : sps
    -- We have run out of spans; add the inserted span to the end of the motion
    -- Don't modify motionFinalAcceleration, which has sketchy semantics to say the least.
    go t f [] = if t == 0
      then f : []
      else spanDurationForce t zero : f : []

seekMotionForward :: (Ord t, Num t) => t -> Motion t q a -> Motion t q a
seekMotionForward cut = motionSpans %~ go cut
  where
    go t (sp:sps) = if
      -- If the cut happens after this span, so we cut this span
      | t >= sp^.spanDuration -> go (t - sp^.spanDuration) sps
      -- If the cut happens during this span, we care
      -- Cut the given time off of this span
      | otherwise -> (spanDuration %~ subtract t) sp : sps
    -- If we run out of spans, that's good!
    -- They've cut to a time beyond our predictions, so our semantics take care of it.
    go _ [] = []

--insertInstant :: (Additive q,Num a,Ord t) => t -> Instant q a -> Motion t q a -> Motion t q a
--insertInstant t i = motionInstants %~ Map.alter (\case {Just a -> Just $ instantDelta %~ (^+^ i^.instantDelta) $ a; Nothing -> Just i}) t
-}
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
{-
sampleFullSpanCollide :: (Metric q, Ord a, Floating a) => Span a q a -> Surface q a -> FreeBody q a -> FreeBody q a
sampleFullSpanCollide sp s fb = case timeOfImpact (sp^.spanForce) s fb of
  Just tCol | tCol < sp^.spanDuration -> sampleForwardForce (sp^.spanForce . to (normalCancel s)) (sp^.spanDuration - tCol) $ fbVelocity %~ normalCancel s $ sampleForwardForce (sp^.spanForce) tCol fb
  _ -> fullSpan sp fb

sampleForceCollide :: (Metric q, Ord a, Floating a) => q a -> Surface q a -> a -> FreeBody q a -> FreeBody q a
sampleForceCollide f s t fb = case timeOfImpact f s fb of
  Just tCol | tCol < t -> sampleForwardForce (normalCancel s f) (t - tCol) $ fbVelocity %~ normalCancel s $ sampleForwardForce f tCol fb
  _ -> sampleForwardForce f t fb

reconcileSpans :: (Additive q, Num t, Ord t, Num a) => Map.Map t (Instant q a) -> (q a,[Span t q a])
reconcileSpans m = go zero splitIndicies
  where
    splitIndicies = Map.assocs m
    go c ((t0,df0):(t1,df1):sps) = _1 .~ c' $ _2 %~ (spanDurationForce (t1 - t0) c':) $ go c' ((t1,df1):sps)
      where c' = c ^+^ df0^.instantDelta
    go c ((_tf,dff):[]) = (c',spanDurationForce 0 c' : [])
      where c' = c ^+^ dff^.instantDelta
    go c [] = (c,[])

sampleMultispanCollide :: (Metric q, Ord a, Floating a) => [Span a q a] -> q a -> Surface q a -> a -> FreeBody q a -> FreeBody q a
sampleMultispanCollide (sp:sps) ff s t fb
  | t >= sp^.spanDuration = sampleMultispanCollide sps ff s (t - sp^.spanDuration) (sampleFullSpanCollide sp s fb)
  | otherwise = sampleForceCollide (sp^.spanForce) s t fb
sampleMultispanCollide [] ff s t fb = sampleForceCollide ff s t fb

sampleMotion :: (Metric q, Ord a, Floating a) => Motion a q a -> Surface q a -> a -> FreeBody q a -> FreeBody q a
sampleMotion m = sampleMultispanCollide (m^.motionSpans) (m^.motionFinalAcceleration)

-- Apply the span for its entire duration to the free body
fullSpan :: (Additive q, Fractional a) => Span a q a -> FreeBody q a -> FreeBody q a
fullSpan sp fb = (fbPosition %~ (^+^) (0.5 *^ sp^.spanForce ^* (sp^.spanDuration * sp^.spanDuration))) . (fbVelocity %~ (^+^) (sp^.spanDuration *^ sp^.spanForce)) $ fb

fullSpanDeltaV :: (Functor q, Num a) => Span a q a -> q a
fullSpanDeltaV sp = sp^.spanDuration *^ sp^.spanForce

-- Apply an acceleration for an amount of time and simulate the resulting free motion
sampleForwardForce :: (Additive q, Fractional a) => q a -> a -> FreeBody q a -> FreeBody q a
sampleForwardForce f t fb = (fbPosition %~ (^+^) ((fb^.fbVelocity ^* t) ^+^ 0.5 *^ f ^* (t * t))) . (fbVelocity %~ (^+^) (t *^ f)) $ fb

--freeMotion :: (Num a, Additive q) => Motion t q a
--freeMotion = Motion
  --{_motionSpans = []
  --,_motionFinalAcceleration = zero
  --}

--motionFromTimeline :: (Num a, Additive q) => [Span t q a] -> Motion t q a
--motionFromTimeline tl = motionSpans .~ tl $ freeMotion

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

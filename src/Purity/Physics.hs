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
import Data.Semigroup
import Debug.Trace
import Purity.Data

kindaCloseToZero :: (Ord a,Floating a) => a -> Bool
kindaCloseToZero = (< 0.001) . abs

newtype Surface q a = Surface
  {_surfaceNormal :: q a -- Surface is at and normal to tip of vector
  } deriving (Eq,Ord)
makeLenses ''Surface

groundSurface :: Num a => Surface V3 a
groundSurface = Surface (V3 0 1 0)

-- A physical subset of q a describing the shape of our object
data PhysDomain q a
  = AABBDomain (q a) deriving (Eq,Show)
  -- | SphereDomain a
  -- Origin is lower edge
  -- Height then radius
  -- CapsuleDomain a a

domainVolume :: (Floating a, Metric q, Foldable q) => PhysDomain q a -> a
domainVolume (AABBDomain size) = product size
--domainVolume (SphereDomain r) = 4/3 * pi * r * r * r
--domainVolume (CapsuleDomain h r) = (4/3 * r + h) * pi * r * r

domainCenter :: (Floating a, Additive q) => PhysDomain q a -> q a
domainCenter (AABBDomain size) = 0.5 *^ size

-- A description of our object's velocity, position, orientation, and shape
data PhysModel q a = PhysModel
  {_physDomain :: PhysDomain q a
  ,_currentVelocity :: q a
  ,_currentOrigin :: q a
  ,_currentOrientation :: Quaternion a
  } deriving Eq
makeLenses ''PhysModel

instance (Show a, Show (q a)) => Show (PhysModel q a) where
  show model = blue "[" ++ green ("At: " ++ show (model^.currentOrigin)) ++ blue " | " ++ green ("Vel: " ++ show (model^.currentVelocity)) ++ blue " | " ++ "Look: " ++ show (model^.currentOrientation) ++ blue " | " ++ "Dom: " ++ show (model^.physDomain) ++ blue "]"

-- Where the object is now, plus the applied force
data PhysFuture q a = PhysFuture
  {_presentModel :: PhysModel q a
  ,_appliedForce :: q a
  } deriving Eq
makeLenses ''PhysFuture

-- We blinked and saw the object at a specific time with a specific force applied
data PhysBlink q a = PhysBlink
  {_blinkVision :: PhysFuture q a
  ,_blinkTime :: a
  } deriving Eq
makeLenses ''PhysBlink

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

glideForever :: PhysBlink q a -> PhysGlide q a
glideForever b = PhysGlide b Nothing

applyForce :: q a -> PhysModel q a -> PhysFuture q a
applyForce f m = PhysFuture {_presentModel = m, _appliedForce = f}

instance (Show a, Show (q a)) => Show (PhysFuture q a) where
  show f = green "{" ++ green ("Apply " ++ show (f^.appliedForce)) ++ " to " ++ show (f^.presentModel) ++ green "}"

-- The object as it stands, plus where it will be forevermore
data PhysStory q a = PhysStory
  {_initialModel :: PhysFuture q a
  -- Nothing means this future continues forever
  -- Just means this future applies for a short time, then there is another chapter
  ,_expiryInfo :: Maybe (a,PhysStory q a)
  }
makeLenses ''PhysStory

--expiryTime :: Traversal' (PhysStory q a) a
--expiryTime = expiryInfo._Just._1

nextChapter :: Traversal' (PhysStory q a) (PhysStory q a)
nextChapter = expiryInfo._Just._2

data PhysDestiny q a = PhysDestiny
  {_destinyTTL :: a
  ,_destineddx :: q a
  ,_destineddv :: q a
  ,_destinedda :: q a
  }
makeLenses ''PhysDestiny

instance (Show a, Show (q a)) => Show (PhysStory q a) where
  show s = case s^.expiryInfo of
    Nothing -> "Finally, " ++ show (s^.initialModel)
    Just (expiry,next) -> "For " ++ red (show expiry) ++ " time " ++ show (s^.initialModel) ++ " then\n" ++ show next

seekFuture :: (Additive q, Fractional a) => a -> PhysFuture q a -> PhysFuture q a
seekFuture t f = presentModel .~ readFuture t f $ f

singleChapter :: PhysFuture q a -> PhysStory q a
singleChapter f = PhysStory {_initialModel = f,_expiryInfo = Nothing}

appendChapter :: (Additive q, Fractional a) => a -> q a -> PhysStory q a -> PhysStory q a
appendChapter expiry f s = case s^.expiryInfo of
  Nothing -> expiryInfo .~ (Just (expiry, singleChapter . applyForce f . readFuture expiry $ s^.initialModel)) $ s
  Just (eTime,next) -> appendChapter (expiry - eTime) f next

spliceBump :: (Additive q, Fractional a, Ord a) => a -> q a -> PhysStory q a -> PhysStory q a
spliceBump t dv s = case s^.expiryInfo of
  Just (expiry,_) | t > expiry -> expiryInfo._Just._2 %~ spliceBump (t - expiry) dv $ s
  _ -> expiryInfo .~ Just (t, singleChapter . (presentModel.currentVelocity %~ (^+^ dv)) . seekFuture t $ s^.initialModel) $ s

--translateDomain :: (Num a, Additive q) => q a -> PhysDomain q a -> PhysDomain q a
--translateDomain off (AABB low high) = AABB (low ^+^ off) (high ^+^ off)

translationConstantAccel :: (Fractional a, Additive q) => a -> q a -> q a -> q a
translationConstantAccel dt v a = dt *^ v ^+^ 0.5 * dt * dt *^ a

velocityConstantAccel :: (Fractional a, Additive q) => a -> q a -> q a -> q a
velocityConstantAccel dt v a = v ^+^ dt *^ a

-- Sample forward the PhysFuture to a time dt after it was blinked
readFuture :: (Fractional a, Additive q) => a -> PhysFuture q a -> PhysModel q a
readFuture dt f = updateVel . updatePos $ f^.presentModel
  where
    updateVel = currentVelocity %~ (^+^ dt *^ f^.appliedForce)
    updatePos = currentOrigin %~ (^+^ translationConstantAccel dt (f^.presentModel.currentVelocity) (f^.appliedForce))

-- Extrapolate a blink forward by dt
reblink :: (Additive q,Fractional a,Num a) => a -> PhysBlink q a -> PhysBlink q a
reblink dt = (blinkVision %~ seekFuture dt) . (blinkTime +~ dt)

-- Extrapolate a blink to an absolute time
blinkTo :: (Additive q,Fractional a,Num a) => a -> PhysBlink q a -> PhysBlink q a
blinkTo t b = (blinkVision %~ seekFuture (t - (b^.blinkTime))) . (blinkTime .~ t) $ b

readStory :: (Fractional a, Additive q, Ord a) => a -> PhysStory q a -> PhysModel q a
readStory dt s = case s^.expiryInfo of
  Just (expiry,next) | dt > expiry -> readStory (dt - expiry) next
  _ -> readFuture dt (s^.initialModel)

--domainsIntersect :: (Applicative q, Foldable q, Metric q, Ord a, Floating a) => q a -> PhysDomain q a -> q a -> PhysDomain q a -> Bool
--domainsIntersect offA (AABBDomain lowA highA) offB (AABBDomain lowB highB) = vecBelow (lowA ^+^ offA) (offB ^+^ highB) || vecBelow (lowB ^+^ offB) (offA ^+^ highA)
--domainsIntersect offA (SphereDomain rA) offB (SphereDomain rB) = distance offA offB < rA + rB
--domainsIntersect offA (SphereDomain rA) offB (AABBDomain lowB highB) = or $ interOn <$> offA <*> offB <*> lowB <*> highB
  --where
    --interOn a b l h = a + rA > b + l || a - rA < b + h
--domainsIntersect a aabb@(AABBDomain _ _) b sph@(SphereDomain _) = domainsIntersect b sph a aabb

vecBelow :: (Applicative q, Foldable q, Ord a) => q a -> q a -> Bool
vecBelow a b = and $ (<) <$> a <*> b

-- Returns the first time the two simultaneous futures intersect after they start
-- This operation is symmetric (anti-symmetry is a bug)
timeOfIntersection :: (Foldable q, Applicative q, Show a, Ord a, Floating a, Metric q) => PhysFuture q a -> PhysFuture q a -> Maybe a
timeOfIntersection fa fb = foldl f Nothing . rectifyQuads $ eqsOfMotion fa fb
  where
    f (Just x) (Just x') = Just $ min x x'
    f Nothing (Just x') = Just x'
    f (Just x) Nothing = Just x
    f Nothing Nothing = Nothing

-- Returns the first (wrt absolute time) intersection of the two blinks, in the form of the first blink forwarded to immediately after the intersection
blinkIntersect :: (Epsilon a,Foldable q, Applicative q, Show (q [a]), Show a, Ord a, Floating a, Metric q) => PhysBlink q a -> PhysBlink q a -> Maybe (PhysBlink q a)
blinkIntersect ba bb = fixup . (baseTime +) <$> timeOfIntersection fa fb
  where
    (fa,fb) = if (ba^.blinkTime) > (bb^.blinkTime)
      then (ba^.blinkVision, seekFuture blinkDrift (bb^.blinkVision))
      else (seekFuture blinkDrift (ba^.blinkVision), bb^.blinkVision)
    blinkDrift = abs $ (ba^.blinkTime) - (bb^.blinkTime)
    baseTime = max (ba^.blinkTime) (bb^.blinkTime)
    fixup t = blinkVision.presentModel .~ resultCollisionModel (blinkTo t bb^.blinkVision.presentModel) (blinkTo t ba^.blinkVision.presentModel) $ blinkTo t ba

maybeMin :: Ord a => V2 (Maybe a) -> Maybe a
maybeMin (V2 (Just a) (Just b)) = Just $ min a b
maybeMin (V2 (Just a) _) = Just a
maybeMin (V2 _ x) = x

intervalOfIntersection :: (Foldable q, Applicative q, Show a, Ord a, Floating a, Metric q) => PhysFuture q a -> PhysFuture q a -> q (Quad a,V2 a)
intervalOfIntersection fa fb = soln 
  <$> fa^.appliedForce
  <*> fb^.appliedForce
  <*> fa^.presentModel.currentVelocity
  <*> fb^.presentModel.currentVelocity
  <*> fa^.presentModel.currentOrigin
  <*> fb^.presentModel.currentOrigin
  <*> qA
  <*> qB
  where
    (AABBDomain qA) = fa^.presentModel.physDomain
    (AABBDomain qB) = fb^.presentModel.physDomain
    soln aA' aB' vA' vB' xA' xB' qA' qB' = (V3 (xB' - xA') (vB' - vA') ((aB' - aA')/2),V2 (-qB') qA')

-- Returns the dim q many quadratics with bounds representing collisions on each axis of the two PhysFutures
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

quadLowerBound :: (Ord a,Floating a,Show a) => a -> a -> a -> Maybe a
quadLowerBound a b c = if a == 0
  then if b == 0
    then if c < 0
      then Just 0 -- Constantly colliding
      else Nothing -- Never colliding
    else if b > 0
      then Just $ max 0 (-c/b) -- From -c/b to forever
      else if (-c/b) > 0 -- From start of time to -c/b
        then Just 0
        else Nothing
  else if det >= 0 
    then let (m,p) = ((-b - sqrt det)/(2 * a),(-b + sqrt det)/(2 * a)) in if p < 0
      then if a > 0
        then Nothing
        else Just 0
      else Just $ max 0 m
    else if a > 0
      then Nothing -- Swoop never collide
      else Just 0 -- Swoop always collide
  where
    det = b * b - 4 * a * c

data Sign = Plus | Minus | Zero deriving (Eq,Show)

signOf :: (Ord a, Num a) => a -> Sign
signOf x = if x > 0 then Plus else (if x < 0 then Minus else Zero)

oppSign :: Sign -> Sign
oppSign Plus = Minus
oppSign Minus = Plus
oppSign Zero = Zero

multSign :: Sign -> Sign -> Sign
multSign Plus = id
multSign Minus = oppSign
multSign Zero = const Zero

paritySign :: Integral a => a -> Sign
paritySign a = if odd a then Minus else Plus

{-
polyLeftSign :: (Ord a, Num a, Eq a) => [a] -> Sign
polyLeftSign xs = case cleanPoly of
  [] -> Zero -- Was actually zero
  (x:_) -> multSign (paritySign order) (signOf x)
  where
    order = length cleanPoly - 1
    cleanPoly = dropWhile (== 0) xs -- Poly with no leading zeros
-}

quadLeftSign :: (Num a, Ord a) => Quad a -> Sign
quadLeftSign (V3 c b a) = case signOf a of
  Zero -> case signOf b of
    Zero -> signOf c
    x -> x
  x -> x

quadOddRoots :: (Ord a, Floating a) => Quad a -> [a]
quadOddRoots (V3 c b a) = case signOf det of
  Plus -> case signOf a of
    Plus -> [(-b - sqrt det)/(2 * a),(-b + sqrt det)/(2 * a)]
    Minus -> [(-b + sqrt det)/(2 * a),(-b - sqrt det)/(2 * a)]
    Zero -> linOddRoots c b -- Was actually linear
  _ -> [] -- Either no roots, or only an even one
  where
    det = b * b - 4 * a * c

evalQuad :: Num a => a -> Quad a -> a
evalQuad x (V3 c b a) = a * x * x + b * x + c

-- Takes a list of quadratic equations with a single codomain bounding interval each, and returns the first time they all satisfy the bounds
rectifyQuads :: (Functor q, Foldable q, Ord a, Floating a, Show a) => q (Quad a, V2 a) -> q (Maybe a)
rectifyQuads qs = fmap (safeHead . dropWhile (not . condition . traceShowId)) mergedRoots
  where
    condition x = and $ fmap (\(q,V2 low high) -> let e = evalQuad x q in (e <= high && low <= e) || kindaCloseToZero (e - high) || kindaCloseToZero (e - low)) qs
    -- q [Quad q a]
    zqs = fmap (\(q,V2 low high) -> [q ^-^ V3 low 0 0,q ^-^ V3 high 0 0]) $ qs
    -- q [[a]]
    allRoots = fmap (fmap quadOddRoots) zqs
    -- q [a]
    mergedRoots = fmap ({-(0:) . -}dropWhile (< 0) . foldr merge []) allRoots

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

clampToZero :: (Ord a, Num a) => Bool -> [a] -> [a]
clampToZero b (x:xs) = if x < 0 then clampToZero True xs else (if b then (0:xs) else xs)
clampToZero _ [] = [0]

catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just x) -> x) . filter (\x -> case x of {Just _ -> True; Nothing -> False})

flipsBefore :: Ord a => a -> [a] -> Sign
flipsBefore t (x:xs) = if t >= x
  then oppSign (flipsBefore t xs)
  else Plus
flipsBefore _ [] = Plus

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | (x <= y)  = x:(merge xs (y:ys)) 
  | otherwise = y:(merge (x:xs) ys)

type Quad = V3

getBoundedQuads :: Num a => Quad a -> V2 a -> V2 (Quad a)
getBoundedQuads p bounds = (^+^ p) . constantQuad . negate <$> bounds

constantQuad :: Num a => a -> Quad a
constantQuad x = V3 x 0 0

-- bx + c = 0 => x = -c/b
linOddRoots :: (Eq a, Fractional a) => a -> a -> [a]
linOddRoots c b = if b == 0 then [] else [-c/b]

turningPoint :: (Metric q, Floating a) => PhysFuture q a -> a
turningPoint f = norm (f^.presentModel.currentVelocity) / norm (f^.appliedForce)

domainAtRest :: (Epsilon a,Additive q, Floating a) => q a -> PhysDomain q a -> PhysModel q a
domainAtRest x0 dom = PhysModel
  {_physDomain = dom
  ,_currentOrigin = x0
  ,_currentVelocity = zero
  ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
  }

gravityFuture :: PhysModel V3 Float -> PhysFuture V3 Float
gravityFuture = applyForce (V3 0 (-9.8) 0)
    
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

fallingStory :: PhysStory V3 Float
fallingStory = PhysStory
  {_initialModel = objFalling
  ,_expiryInfo = Nothing
  }

objLeft :: PhysFuture V3 Float
objLeft = PhysFuture
  {_appliedForce = V3 (-1) 0 0
  ,_presentModel = PhysModel
    {_physDomain = AABBDomain (V3 2 2 2)
    ,_currentOrigin = V3 (-5) (-5) 0
    ,_currentVelocity = V3 20 5 0
    ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
    }
  }

objRight :: PhysFuture V3 Float
objRight = PhysFuture
  {_appliedForce = V3 1 0 0
  ,_presentModel = PhysModel
    {_physDomain = AABBDomain (V3 2 2 2)
    ,_currentOrigin = V3 5 (-5) 0
    ,_currentVelocity = V3 (-20) 5 0
    ,_currentOrientation = axisAngle (V3 0 0 (-1)) 0
    }
  }
{-
worryAbout :: (Foldable q, Metric q, Applicative q, Show a, Ord a, Floating a) => a -> PhysFuture q a -> PhysFuture q a -> PhysModel q a
worryAbout dt fa fb = case mfilter (< dt) (timeOfIntersection fa fb) of
  Nothing -> readFuture dt fa
  Just cTime -> readFuture (dt - cTime) . applyForce (fa^.appliedForce ^+^ normalResponse (readFuture cTime fa) (readFuture cTime fb)) $ readFuture cTime fa

segmentStoryOnCollision :: (Floating a, Metric q, Foldable q, Applicative q, Show a, Ord a) => PhysStory q a -> PhysFuture q a -> PhysStory q a
segmentStoryOnCollision st col = case st^.expiryInfo of
  Nothing -> case timeOfIntersection (st^.initialModel) col of
    -- No collision in this, the last chapter of the story
    Nothing -> st
    Just cTime -> let atCol = readFuture cTime in st {_expiryInfo = Just (cTime,PhysStory
      {_initialModel = presentModel .~ (currentVelocity %~ (^+^ normalResponse (atCol (st^.initialModel)) (atCol col)) $ atCol (st^.initialModel)) $ st^.initialModel
      ,_expiryInfo = Nothing
      })}
  Just (splitTime, st') -> case timeOfIntersection (st^.initialModel) col of
    -- Colliding during *this* chapter
    Just cTime | cTime < splitTime -> let atCol = readFuture cTime in expiryInfo .~ Just (cTime,PhysStory
      {_initialModel = presentModel .~ (currentVelocity %~ (^+^ normalResponse (atCol (st^.initialModel)) (atCol col)) $ atCol (st^.initialModel)) $ st^.initialModel
      ,_expiryInfo = Just (splitTime - cTime, st')
      }) $ st
    -- Not colliding during this chapter, maybe later
    _ -> st {_expiryInfo = Just (splitTime,segmentStoryOnCollision st' col)}

collideWith :: (Floating a, Metric q, Foldable q, Applicative q, Show a, Ord a) => PhysFuture q a -> PhysStory q a -> PhysStory q a
collideWith col st = case timeOfIntersection (st^.initialModel) col of
  -- Perhaps we will collide in a later chapter
  Nothing -> checkNextChapter
  Just cTime -> case st^.expiryInfo of
    Just (expiry,next) | cTime < expiry -> expiryInfo._Just .~ (cTime, PhysStory
      {_initialModel = (presentModel %~ resultCollisionModel (readFuture cTime col)) . seekFuture cTime $ st^.initialModel
      ,_expiryInfo = Just (expiry - cTime,next)}) $ st
    Nothing -> expiryInfo .~ Just (cTime, singleChapter . (presentModel %~ resultCollisionModel (readFuture cTime col)) . seekFuture cTime $ st^.initialModel) $ st
    _ -> checkNextChapter
  where
    checkNextChapter = expiryInfo._Just %~ (\(expiry,next) -> (expiry,collideWith (seekFuture expiry col) next)) $ st
-}
resultCollisionModel :: (Epsilon a,Ord a,Floating a, Metric q) => PhysModel q a -> PhysModel q a -> PhysModel q a
resultCollisionModel col move = (currentOrigin %~ (^+^ 0.05 *^ normalResponse move col)) . (currentVelocity %~ (^+^ normalResponse move col)) $ move

normalResponse :: (Epsilon a,Ord a,Floating a, Metric q) => PhysModel q a -> PhysModel q a -> q a
normalResponse a b = normalize (collisionNormal b a) ^* ((1 + norm dv) * 1.6)
  where
    dv = a^.currentVelocity ^-^ b^.currentVelocity

-- Points from B to A (on centers)
collisionNormal :: (Ord a,Floating a,Additive q) => PhysModel q a -> PhysModel q a -> q a
collisionNormal a b = topA ^+^ topB --(a^.currentOrigin ^+^ a^.physDomain.to domainCenter) ^-^ (b^.currentOrigin ^+^ b^.physDomain.to domainCenter)
  where
    topA = fmap (\x -> if kindaCloseToZero x then 1 else 0) $ (a^.currentOrigin ^+^ qA) ^-^ (b^.currentOrigin)
    topB = fmap (\x -> if kindaCloseToZero x then (-1) else 0) $ (a^.currentOrigin) ^-^ (b^.currentOrigin ^+^ qB)
    (AABBDomain qA) = a^.physDomain
    (AABBDomain qB) = b^.physDomain

nextBlink :: (Epsilon a,Show (q [a]),Eq (q a),Foldable q, Applicative q, Show a, Ord a, Floating a, Metric q) => [PhysBlink q a] -> PhysBlink q a -> Maybe (PhysBlink q a)
nextBlink bs b = fmap getMin . getOption . foldMap (Option . fmap Min) $ blinkIntersect b <$> (filter (/= b) bs)

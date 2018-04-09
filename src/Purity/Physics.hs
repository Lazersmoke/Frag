{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Purity.Physics where

import Linear
import Control.Lens
import Control.Monad
import Data.Foldable
--import Debug.Trace
import Purity.Data

newtype Surface q a = Surface
  {_surfaceNormal :: q a -- Surface is at and normal to tip of vector
  } deriving (Eq,Ord)
makeLenses ''Surface

groundSurface :: Num a => Surface V3 a
groundSurface = Surface (V3 0 1 0)

data PhysDomain q a
  = AABBDomain (q a) deriving Show
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

data PhysModel q a = PhysModel
  {_physDomain :: PhysDomain q a
  ,_currentVelocity :: q a
  ,_currentOrigin :: q a
  ,_currentOrientation :: Quaternion a
  }
makeLenses ''PhysModel

instance (Show a, Show (q a)) => Show (PhysModel q a) where
  show model = "[" ++ green ("At: " ++ show (model^.currentOrigin)) ++ " | " ++ green ("Vel: " ++ show (model^.currentVelocity)) ++ " | Look: " ++ show (model^.currentOrientation) ++ " | Dom: " ++ show (model^.physDomain) ++ "]"

data PhysFuture q a = PhysFuture
  {_presentModel :: PhysModel q a
  ,_appliedForce :: q a
  }
makeLenses ''PhysFuture

applyForce :: q a -> PhysModel q a -> PhysFuture q a
applyForce f m = PhysFuture {_presentModel = m, _appliedForce = f}

instance (Show a, Show (q a)) => Show (PhysFuture q a) where
  show f = "{" ++ green ("Apply " ++ show (f^.appliedForce)) ++ " to " ++ show (f^.presentModel) ++ "}"

data PhysStory q a = PhysStory
  {_initialModel :: PhysFuture q a
  ,_expiryInfo :: Maybe (a,PhysStory q a)
  }
makeLenses ''PhysStory

expiryTime :: Traversal' (PhysStory q a) a
expiryTime = expiryInfo._Just._1

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
  Just (expiry,next) -> if t > expiry
    then expiryInfo .~ Just (expiry, spliceBump (t - expiry) dv next) $ s
    else expiryInfo .~ Just (t, singleChapter . (presentModel.currentVelocity %~ (^+^ dv)) . seekFuture t $ s^.initialModel) $ s
  Nothing -> s

--translateDomain :: (Num a, Additive q) => q a -> PhysDomain q a -> PhysDomain q a
--translateDomain off (AABB low high) = AABB (low ^+^ off) (high ^+^ off)

translationConstantAccel :: (Fractional a, Additive q) => a -> q a -> q a -> q a
translationConstantAccel dt v a = dt *^ v ^+^ 0.5 * dt * dt *^ a

velocityConstantAccel :: (Fractional a, Additive q) => a -> q a -> q a -> q a
velocityConstantAccel dt v a = v ^+^ dt *^ a

readFuture :: (Fractional a, Additive q) => a -> PhysFuture q a -> PhysModel q a
readFuture dt f = updateVel . updatePos $ f^.presentModel
  where
    updateVel = currentVelocity %~ (^+^ dt *^ f^.appliedForce)
    updatePos = currentOrigin %~ (^+^ translationConstantAccel dt (f^.presentModel.currentVelocity) (f^.appliedForce))

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

timeOfIntersection :: (Foldable q, Applicative q, Show a, Ord a, Floating a, Metric q) => PhysFuture q a -> PhysFuture q a -> Maybe a
timeOfIntersection fa fb = rectifyQuads . toList $ intervalOfIntersection fa fb

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

quadOddRoots :: (Show a, Ord a, Floating a) => Quad a -> [a]
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

rectifyQuads :: (Show a, Ord a, Floating a) => [(Quad a, V2 a)] -> Maybe a
rectifyQuads qs = safeHead . dropWhile (not . condition) $ mergedRoots
  where
    --initSigns = fmap quadLefSign qs
    --roots = snd . foldr (\xs (n,l) -> (n + 1,merge l n xs)) (0,[]) . fmap (clampToZero False) . traceShowId $ allRoots
    condition x = and $ fmap (\(q,V2 low high) -> let e = evalQuad x q in e <= high && low <= e) qs
    zqs = concatMap (\(q,V2 low high) -> [q ^-^ V3 low 0 0,q ^-^ V3 high 0 0]) $ qs
    allRoots = fmap quadOddRoots zqs
    mergedRoots = dropWhile (< 0) . foldr merge [] $ allRoots

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
    
objAtZero :: PhysFuture V3 Float
objAtZero = PhysFuture
  {_appliedForce = zero
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

worryAbout :: (Foldable q, Metric q, Applicative q, Show a, Ord a, Floating a, Epsilon a) => a -> PhysFuture q a -> PhysFuture q a -> PhysModel q a
worryAbout dt fa fb = case mfilter (< dt) (timeOfIntersection fa fb) of
  Nothing -> readFuture dt fa
  Just cTime -> readFuture (dt - cTime) . applyForce (fa^.appliedForce ^+^ normalResponse (readFuture cTime fa) (readFuture cTime fb)) $ readFuture cTime fa

segmentStoryOnCollision :: (Floating a, Metric q, Epsilon a, Foldable q, Applicative q, Show a, Ord a) => PhysStory q a -> PhysFuture q a -> PhysStory q a
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

collideWith :: (Floating a, Metric q, Epsilon a, Foldable q, Applicative q, Show a, Ord a) => PhysFuture q a -> PhysStory q a -> PhysStory q a
collideWith col st = case timeOfIntersection (st^.initialModel) col of
  -- Perhaps we will collide in a later chapter
  Nothing -> expiryInfo._Just %~ (\ei -> (_2 %~ collideWith (seekFuture (ei^._1) col)) ei) $ st
  Just cTime -> case st^.expiryInfo of
    Nothing ->
    Just (expiry,next) ->

normalResponse :: (Floating a, Epsilon a, Metric q) => PhysModel q a -> PhysModel q a -> q a
normalResponse mover kicker = normalize kickNormal ^* (norm dv * 1.6)
  where
    dv = mover^.currentVelocity ^-^ kicker^.currentVelocity
    kickNormal = (mover^.currentOrigin ^+^ mover^.physDomain.to domainCenter) ^-^ (kicker^.currentOrigin ^+^ kicker^.physDomain.to domainCenter)


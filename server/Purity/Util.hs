{-# LANGUAGE FlexibleContexts #-}
module Purity.Util where

import Purity.Data 

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Concurrent

testServerState :: ServerState
testServerState = addObject oneCube 
  {wish = Vector (1,0,0)
  ,vel = Vector (-1,-1,-1)
  } $ freshServerState 
    {world = 
      World
      {geometry = 
        [WorldPlane (Vector (-5,-2,-5), Vector (-5,-2,5), Vector (5,-2,5))
        ,WorldPlane (Vector (-5,-2,-5), Vector (5,-2,5), Vector (5,-2,-5))
        ,WorldPlane (Vector (-5,10, 5), Vector (-5,-2, 5), Vector (-5,-2,-5))
        ]
      ,levelName = "testlevel"
      }
  }

-- # Monad Generics # --

-- Pass an argument to two different actions, compose with >>, return second
tee :: Monad m => (a -> m c) -> (a -> m b) -> a -> m b
tee first second arg = first arg >> second arg

-- Switch on a bool, less pointful
switch :: a -> a -> Bool -> a
switch yay nay sw = if sw then yay else nay

-- Apply a list of homomorphisms in order
listApFold :: [a -> a] -> a -> a
listApFold = foldl (flip (.)) id 

-- # MonadReader (MVar a) Helpers # --

-- Send a message in an IO Monad
sendMessage :: WS.Connection -> String -> IO ()
sendMessage conn = WS.sendTextData conn . T.pack

-- Get a message and unpack it, in IO Monad
receiveMessage :: WS.Connection -> IO String
receiveMessage conn = T.unpack <$> WS.receiveData conn

-- Send a message to a player
sendMessagePlayer :: Player -> String -> IO ()
sendMessagePlayer pla = sendMessage (connection pla) 

-- Parse a String to a UC by stamping it with the tick
parseUC :: MVar ServerState -> String -> IO UserCommand
parseUC ss text = readMVar ss >>= \s -> return UserCommand {tick = currentTick s, command = text}

-- Receive a message from a player
receiveMessagePlayer :: Player -> IO String
receiveMessagePlayer pla = receiveMessage (connection pla) 

addConnAsPlayer :: MVar ServerState -> WS.Connection -> PlayerStatus -> IO Player
addConnAsPlayer ss conn ps = do
  -- Ask client for their name
  sendMessage conn "What is your name?"
  -- Wait for client to give their name
  chosenName <- receiveMessage conn
  -- Validate the chosen name and switch over it
  validatePlayerName chosenName >>= switch
    -- If valid
    (tee
      (\p -> modifyMVar_ ss (return . addPlayer p)) -- Add it to the state
      return -- And return it
      Player { -- Make a new player
        name = chosenName, -- With the chosen name
        connection = conn,
        status = ps,
        userCmds = [],
        ready = False,
        object = emptyObject {size = Vector (0.1,0.1,0.1)}
        }
    )
    -- If Invalid, ask again
    (addConnAsPlayer ss conn ps)
    where
      -- Not in current player list and less than 50 long
      validatePlayerName chosen = fmap ((&& length chosen < 50) . notElem chosen . map name . players) (readMVar ss)

-- # ServerState Player Manip # --
updateReady :: Player -> Player
updateReady p
  | "Ready" `elem` map command (userCmds p) = p {ready = True}
  | "Unready" `elem` map command (userCmds p) = p {ready = False}
  | otherwise = p

-- # ServerState Object Manip

-- Add a object to an existing ServerState
addObject :: Object -> ServerState -> ServerState
addObject obj ss = ss {objects = obj : objects ss}

-- Drop an object
dropObject :: Object -> ServerState -> ServerState
dropObject obj ss = ss {objects = delete obj $ objects ss}

-- # ServerState Other Manip # --

-- Increment the Tick
incrementTick :: ServerState -> ServerState
incrementTick ss = ss {currentTick = currentTick ss + 1}

startGame :: ServerState -> ServerState
startGame ss = ss {phase = Playing, players = map (setStatus Respawning) (players ss)}


-- Tell a connection about the Game Phase
tellGamePhase :: MVar ServerState -> WS.Connection -> IO ()
tellGamePhase ss = tellConnection ss $ ("Game Phase is " ++) . show . phase 

-- Tell a connection the list of players
tellPlayerList :: MVar ServerState -> WS.Connection -> IO ()
tellPlayerList ss = tellConnection ss $ show . map name . players

-- Tell a player something about the state
tellConnection :: MVar ServerState -> (ServerState -> String) -> WS.Connection -> IO ()
tellConnection ss f conn = 
  WS.sendTextData conn -- Then send it
  . T.pack -- Pack it into a text for sending
  . f -- Apply the user transform
  =<< readMVar ss -- Grab the current game state

---------------------
-- # Vector Math # --
---------------------

-- get magnitude of a vector (distance formula)
magnitude :: Vector -> VectorComp
magnitude (Vector (a,b,c)) = sqrt $ a**2 + b**2 + c**2

-- Scale by a number (prefix synonym for (*))
scale :: VectorComp -> Vector -> Vector
scale s = (Vector (s,s,s) *) 

-- Dot product = sum of product of corresponding components
dotProduct :: Vector -> Vector -> VectorComp
dotProduct (Vector (ax,ay,az)) (Vector (bx,by,bz)) = (ax*bx)+(ay*by)+(az*bz)

-- Right hand rule thing
crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector (a1,a2,a3)) (Vector (b1,b2,b3)) = 
  Vector 
  (
  a2 * b3 - a3 * b2,
  a3 * b1 - a1 * b3,
  a1 * b2 - a2 * b1
  )

-- Force the sum to be 1
normalizeVector :: Vector -> Vector
normalizeVector vec = scale cleansed vec 
  where
    cleansed = if magnitude vec > 0 then 1 / magnitude vec else 0

normalWP :: WorldPlane -> Vector 
normalWP (WorldPlane (v1,v2,v3)) = normalizeVector v
  where
    p1 = v2 - v1
    p2 = v3 - v1
    v = crossProduct p1 p2 

facingDir :: Object -> Vector
facingDir o = rotatePitchYaw (pitch o) (yaw o) (Vector (0,0,1))

asRadians :: Floating a => a -> a
asRadians = (* pi) . (/180)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- TODO: add genPlayerMessage to add player info. also update on client to accept new format
generateMessage :: ServerState -> String
generateMessage s = 
  "{\"objects\":" ++ genListMessage genObjMessage (objects s) 
  ++ ", \"players\": " ++ genListMessage genObjMessage (map object $ players s) 
  ++ "}"
  where
    genListMessage xf xs = "[" ++ (intercalate "," . map xf $ xs) ++ "]"
    genObjMessage obj = 
      "{\"pos\":" ++ genVecMessage (pos obj) 
      ++ ",\"size\":" ++ genVecMessage (size obj) 
      ++ ",\"dir\":" ++ genVecMessage (facingDir obj) 
      ++ "}"
    genVecMessage v = 
      "{\"x\":" ++ show (vecX v) 
      ++ ",\"y\":" ++ show (vecY v) 
      ++ ",\"z\":" ++ show (vecZ v) 
      ++ "}"

parseVector :: String -> Maybe Vector
parseVector str =
    case map maybeRead w of
      [Just x, Just y, Just z] -> Just $ Vector (x, y, z)
      _ -> Nothing
  where
    w = words str

-- Credit to /u/Taylee <3
rotatePitchYaw :: VectorComp -> VectorComp -> Vector -> Vector
rotatePitchYaw dPitch dYaw (Vector (x,y,z)) = 
  Vector
  (cos dYaw * x + sin dYaw * (sin dPitch * y + cos dPitch * z)
  ,cos dPitch * y - sin dPitch * z
  ,- sin dYaw * x + cos dYaw * (sin dPitch * y + cos dPitch * z)
  )

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

project :: [Vector] -> Vector -> [VectorComp]
project verts vec = map (dotProduct vec) verts

projectAABB :: AABB -> Vector -> [VectorComp]
projectAABB aabb = project (aabbCorners aabb)

projectWP :: WorldPlane -> Vector -> [VectorComp]
projectWP (WorldPlane (v1,v2,v3)) = project [v1,v2,v3]

areSeparated :: Ord a => [a] -> [a] -> Bool
areSeparated a b = maximum a < minimum b || minimum a > maximum b

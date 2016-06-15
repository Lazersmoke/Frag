{-# LANGUAGE FlexibleContexts #-}
module Purity.Util where

import Purity.Data 

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Concurrent

testObject :: Object
testObject = (wish >@> Vector (1,0,0)) . (vel >@> Vector (-1,-1,-1)) $ oneCube

testWorld :: World
testWorld = World
  {geometry = 
    [WorldPlane (Vector (-5,-2,-5), Vector (-5,-2,5), Vector (5,-2,5))
    ,WorldPlane (Vector (-5,-2,-5), Vector (5,-2,5), Vector (5,-2,-5))
    ,WorldPlane (Vector (-5,10, 5), Vector (-5,-2, 5), Vector (-5,-2,-5))
    ]
  ,levelName = "testlevel"
  }

testServerState :: ServerState
testServerState = (objects >&> (testObject:)) . (world >@> testWorld) $ freshServerState 

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
sendMessagePlayer pla = sendMessage (connection ~>> pla) 

-- Parse a String to a UC by stamping it with the tick
parseUC :: MVar ServerState -> String -> String -> IO UserCommand
parseUC ss text plaName = readMVar ss >>= \s -> return $ UserCommand (currentTick ~>> s) text plaName

-- Receive a message from a player
receiveMessagePlayer :: Player -> IO String
receiveMessagePlayer pla = receiveMessage (connection ~>> pla) 

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
      (\p -> modifyMVar_ ss (return . change players (p:))) -- Add it to the state
      return $ -- And return it
        Player -- Make a new player
        chosenName -- With the chosen name
        conn
        ps
        (set size (scale 0.1 unitVector) emptyObject)
        False -- Not Ready
        [] -- No commands
    )
    -- If Invalid, ask again
    (addConnAsPlayer ss conn ps)
    where
      -- Not in current player list and less than 50 long
      validatePlayerName chosen = fmap ((&& length chosen < 50) . notElem chosen . map (grab name) . grab players) (readMVar ss)

-- # ServerState Player Manip # --
updateReady :: ServerState -> ServerState
updateReady ss = map (\u -> if command ~>> u == "Ready" then set  -- TODO
  | "Ready" `elem` map (command ~>>) (userCmds ~>> p) = set ready True p 
  | "Unready" `elem` map (command ~>>) (userCmds ~>> p) = set ready False p 
  | otherwise = p

-- # ServerState Object Manip

-- Add a object to an existing ServerState
addObject :: Object -> ServerState -> ServerState
addObject obj = objects >&> (obj:)

-- Drop an object
dropObject :: Object -> ServerState -> ServerState
dropObject obj = objects >&> delete obj

-- # ServerState Other Manip # --

-- Increment the Tick
incrementTick :: ServerState -> ServerState
incrementTick = currentTick >&> (+1)

-- Setup everything to start the game
startGame :: ServerState -> ServerState
startGame = (phase >@> Playing) . changeMap players (status >@> Respawning)


-- Tell a connection about the Game Phase
tellGamePhase :: MVar ServerState -> WS.Connection -> IO ()
tellGamePhase ss = tellConnection ss $ ("Game Phase is " ++) . show . grab phase 

-- Tell a connection the list of players
tellPlayerList :: MVar ServerState -> WS.Connection -> IO ()
tellPlayerList ss = tellConnection ss $ show . map (grab name) . grab players

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
facingDir o = rotObj o forwardVector

rotObj :: Object -> Vector -> Vector
rotObj o = rotatePitchYaw (pitch ~>> o) (yaw ~>> o)

forwardVector :: Vector
forwardVector = Vector (0,0,1)

asRadians :: Floating a => a -> a
asRadians = (* pi) . (/180)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- TODO: add genPlayerMessage to add player info. also update on client to accept new format
generateMessage :: ServerState -> String
generateMessage s = 
  "{\"objects\":" ++ genListMessage genObjMessage (objects ~>> s) 
  ++ ", \"players\": " ++ genListMessage genObjMessage (map (object ~>>) $ players ~>> s) 
  ++ "}"
  where
    genListMessage xf xs = "[" ++ (intercalate "," . map xf $ xs) ++ "]"
    genObjMessage obj = 
      "{\"pos\":" ++ genVecMessage (pos ~>> obj) 
      ++ ",\"size\":" ++ genVecMessage (size ~>> obj) 
      ++ ",\"dir\":" ++ genVecMessage (facingDir obj) 
      ++ "}"
    genVecMessage (Vector (x,y,z)) = 
      "{\"x\":" ++ show x
      ++ ",\"y\":" ++ show y
      ++ ",\"z\":" ++ show z
      ++ "}"

-- Credit to /u/Taylee <3
rotatePitchYaw :: VectorComp -> VectorComp -> Vector -> Vector
rotatePitchYaw dPitch dYaw (Vector (x,y,z)) = 
  Vector
  (cos dYaw * x + sin dYaw * (sin dPitch * y + cos dPitch * z)
  ,cos dPitch * y - sin dPitch * z
  ,- sin dYaw * x + cos dYaw * (sin dPitch * y + cos dPitch * z)
  )

project :: [Vector] -> Vector -> [VectorComp]
project verts vec = map (dotProduct vec) verts

projectAABB :: AABB -> Vector -> [VectorComp]
projectAABB aabb = project (aabbCorners aabb)

projectWP :: WorldPlane -> Vector -> [VectorComp]
projectWP (WorldPlane (v1,v2,v3)) = project [v1,v2,v3]

areSeparated :: Ord a => [a] -> [a] -> Bool
areSeparated a b = maximum a < minimum b || minimum a > maximum b

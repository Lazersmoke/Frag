{-# LANGUAGE FlexibleContexts #-}
module FragUtil where

import FragData 
import FragCommands

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Data.List
import Control.Concurrent
import Control.Monad.Reader

testServerState :: ServerState
testServerState = addObject oneCube {vel = Vector (-1,-1,-1)} $ freshServerState {world = World {geometry = [WorldPlane (setVecX 1 emptyVector, setVecY 1 emptyVector, setVecZ 1 emptyVector)], levelName = ""}}

-- # Monad Generics # --

-- Pass an argument to two different actions, compose with >>, return second
tee :: Monad m => (a -> m c) -> (a -> m b) -> a -> m b
tee first second arg = first arg >> second arg

-- Switch on a bool, less pointful
switch :: a -> a -> Bool -> a
switch yay nay sw = if sw then yay else nay

-- Make this shorter 
io :: MonadIO m => IO a -> m a
io = liftIO

-- # MonadReader (MVar a) Helpers # --

-- Send a message in an IO Monad
sendMessage :: MonadIO m => WS.Connection -> String -> m ()
sendMessage conn = io . WS.sendTextData conn . T.pack

-- Get a message and unpack it, in ConnectionT
receiveMessage :: MonadIO m => WS.Connection -> m String
receiveMessage conn = T.unpack <$> (io . WS.receiveData $ conn)

-- Send a message to a player
sendMessagePlayer :: MonadIO m => Player -> String -> m ()
sendMessagePlayer pla = sendMessage (connection pla) 

-- Parse a String to a UC
parseUC :: (MonadIO m, MonadReader (MVar ServerState) m) => String -> m UserCommand
parseUC text = grabState >>= \s -> return UserCommand {tick = currentTick s, command = text}

-- Receive a message from a player
receiveMessagePlayer :: MonadIO m => Player -> m String
receiveMessagePlayer pla = receiveMessage (connection pla) 

-- Read an MVar in a reader generically
grabState :: (MonadIO m, MonadReader (MVar b) m) => m b
grabState = ask >>= io . readMVar

-- Trasform the MVar/Reader state with IO
transformStateIO :: (MonadIO m, MonadReader (MVar b) m) => (b -> IO b) -> m ()
transformStateIO f = ask >>= io . flip modifyMVar_ f

-- Transform the MVar/Reader state without IO
transformState :: (MonadIO m, MonadReader (MVar b) m) => (b -> b) -> m ()
-- Compose with a return (to make it IO), then give it to transformStateIO
transformState = transformStateIO . (return .)

addConnAsPlayer :: WS.Connection -> PlayerStatus -> ConnectionT Player
addConnAsPlayer conn ps = do
  -- Ask client for their name
  sendMessage conn "What is your name?"
  -- Wait for client to give their name
  chosenName <- receiveMessage conn
  -- Validate the chosen name and switch over it
  validatePlayerName chosenName >>= switch
    -- If valid
    (tee
      (transformState . addPlayer) -- Add it to the state
      return -- And return it
      Player { -- Make a new player
        name = chosenName, -- With the chosen name
        connection = conn,
        status = ps,
        userCmds = [],
        ready = False,
        object = emptyObject
        }
    )
    -- If Invalid, ask again
    (addConnAsPlayer conn ps)
    where
      -- Not in current player list and less than 50 long
      validatePlayerName chosen = fmap ((&& length chosen < 50) . notElem chosen . map name . players) grabState

-- # ServerState Player Manip # --

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
-- Perform all uc's for the player
performUCs :: Player -> ServerState -> ServerState
performUCs p ss = foldl (performUC p) ss ucs
  where
    ucs = userCmds p

-- Tell a connection about the Game Phase
tellGamePhase :: (MonadIO m, MonadReader (MVar ServerState) m) => WS.Connection -> m ()
tellGamePhase = tellConnection $ ("Game Phase is " ++) . show . phase 

-- Tell a connection the list of players
tellPlayerList :: (MonadIO m, MonadReader (MVar ServerState) m) => WS.Connection -> m ()
tellPlayerList = tellConnection $ show . map name . players

-- Tell a player something about the state
tellConnection :: (MonadIO m, MonadReader (MVar ServerState) m) => (ServerState -> String) -> WS.Connection -> m ()
tellConnection f conn = io
  . WS.sendTextData conn -- Then send it
  . T.pack -- Pack it into a text for sending
  . f -- Apply the user transform
  =<< grabState -- Grab the current game state

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
normalizeVector vec = vec / magnitude vec 

repairWPIntersection :: WorldPlane -> Object -> Object
repairWPIntersection wp obj = obj {vel = newvel}
  where
    dp = dotProduct (vel obj) nor -- 1 if away, -1 if towards, 0 if _|_ etc
    newvel = if dp > 0
      then vel obj
      else vel obj + scale (springCo * dp) nor -- questionable method
    nor = normalWP wp
    springCo = 1.1

normalWP :: WorldPlane -> Vector 
normalWP (WorldPlane (v1,v2,v3)) = normalizeVector v
  where
    p1 = v2 - v1
    p2 = v3 - v1
    v = crossProduct p1 p2 

intersectAABBWP :: Vector -> Vector -> WorldPlane -> Bool
intersectAABBWP amin amax wp@(WorldPlane (v1,v2,v3)) = intersectAABBNormal || intersectWPNormal || intersectCross
  where
    wpCorners = [v1,v2,v3]
    wpEdges = [v1-v3,v2-v1,v3-v2]
    wpNormal = normalWP wp

    intersectAABBNormal = any (not . aabbNormalClear) unitVectors
    aabbNormalClear axis = maximum (triProject axis) < minimum (map (extractUnit axis) wpCorners) || minimum (triProject axis) > maximum (map (extractUnit axis) wpCorners)

    project :: [Vector] -> Vector -> [VectorComp]
    project verts vec = map (dotProduct vec) verts
    boxProject = project (aabbCorners amin amax)
    triProject = project wpCorners

    intersectWPNormal = not $ maximum wpProjections < wpNormalOffset || minimum wpProjections > wpNormalOffset
    wpNormalOffset = dotProduct wpNormal v1
    wpProjections = boxProject wpNormal

    intersectCross = or [crossCheck t b | t <- wpEdges, b <- unitVectors]
    crossCheck tri box = maximum (boxCross tri box) < minimum (triCross tri box) || minimum (triCross tri box) > maximum (boxCross tri box)

    boxCross tri box = boxProject (tri `crossProduct` box)
    triCross tri box = triProject (tri `crossProduct` box)


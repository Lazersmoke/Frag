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
testServerState = addObject oneCube {vel = Vector (1,0,0)} . addObject oneCube {pos = Vector (10,0,0)} $ freshServerState

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

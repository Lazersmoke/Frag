{-# LANGUAGE FlexibleContexts #-}
module FragUtil where

import FragData

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Data.List
import Control.Concurrent
import Control.Monad.Reader

-- # Monad Generics # --

-- Pass an argument to two different actions, compose with >>, return second
tee :: Monad m => a -> (a -> m c) -> (a -> m b) -> m b
tee arg first second = first arg >> second arg

-- Switch on a bool, less pointful
switch :: a -> a -> Bool -> a
switch yay nay sw = if sw then yay else nay

-- Make this shorter 
io :: MonadIO m => IO a -> m a
io = liftIO

-- # Connection Helpers # -- 

-- Send a message in an IO Monad
sendMessage :: MonadIO m => WS.Connection -> String -> m ()
sendMessage conn = io . WS.sendTextData conn . T.pack

-- Tell a connection about the Game Stage
tellGameStage :: WS.Connection -> ConnectionT ()
tellGameStage = tellConnection $ ("Game Stage is " ++) . show . stage 

-- Tell a connection the list of players
tellPlayerList :: WS.Connection -> ConnectionT ()
tellPlayerList = tellConnection $ show . map name . players

-- Tell a player something about the state
tellConnection :: (ServerState -> String) -> WS.Connection -> ConnectionT ()
tellConnection f conn = io
  . WS.sendTextData conn -- Then send it
  . T.pack -- Pack it into a text for sending
  . f -- Apply the user transform
  =<< grabState -- Grab the current game state

-- Get a message and unpack it, in ConnectionT
receiveMessage :: WS.Connection -> ConnectionT String
receiveMessage conn = T.unpack <$> (io . WS.receiveData $ conn)

-- # MonadReader (MVar a) Helpers # --

-- Read an MVar in a reader generically
grabState :: (MonadIO m, MonadReader (MVar b) m) => m b
grabState = ask >>= io . readMVar

-- Trasform the MVar/Reader state with IO
transformStateIO :: (MonadIO m, MonadReader (MVar b) m) => (b -> IO b) -> m ()
transformStateIO f = ask >>= io . flip modifyMVar_ f

-- Transform the MVar/Reader state without IO
transformState :: (MonadIO m, MonadReader (MVar b) m) => (b -> b) -> m ()
transformState = transformStateIO . (return .)

addConnAsPlayer :: WS.Connection -> PlayerStage -> ConnectionT Player
addConnAsPlayer conn ps = do
  -- Ask client for their name
  io $ WS.sendTextData conn (T.pack "What is your name?")
  -- Wait for client to give their name
  chosenName <- T.unpack <$> io (WS.receiveData conn)
  -- Validate the chosen name and switch over it
  validatePlayerName chosenName >>= switch
    -- If valid
    (tee
      Player { -- Make a new player
        name = chosenName, -- With the chosen name
        connection = conn,
        playerStage = ps,
        userCmds = []
        }
      (transformState . addPlayer) -- Add it to the state
      return -- And return it
    )
    -- If Invalid, ask again
    (addConnAsPlayer conn ps)
    where
      -- Not in current player list and less than 50 long
      validatePlayerName chosen = fmap ((&& length chosen < 50) . notElem chosen . map name . players) grabState

-- # ServerState Player Manip # --

-- Add a player to an existing ServerState
addPlayer :: Player -> ServerState -> ServerState
addPlayer pla ss = ss {players = pla : players ss}

-- Drop a player
dropPlayer :: Player -> ServerState -> ServerState
dropPlayer pla ss = ss {players = delete pla $ players ss}

-- Change the first player to the second one
modifyPlayer :: Player -> Player -> ServerState -> ServerState
modifyPlayer old new ss = ss {players = new : delete old (players ss)}

-- # Player Helpers # --

-- Send a message to a player
sendPlayerMessage :: MonadIO m => Player -> String -> m ()
sendPlayerMessage pla = io . WS.sendTextData (connection pla) . T.pack

-- Set a player's stage
setPlayerStage :: PlayerStage -> Player -> Player
setPlayerStage ps pla = pla {playerStage = ps}

-- Add a UC to a player
addUC :: UserCommand -> Player -> Player
addUC uc pla = pla {userCmds = uc : userCmds pla}

-- Parse a String to a UC
parseUC :: String -> ConnectionT UserCommand
parseUC text = grabState >>= \s -> return UserCommand {tick = currentTick s, command = text}


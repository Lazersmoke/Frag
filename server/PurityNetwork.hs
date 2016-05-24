{-# LANGUAGE OverloadedStrings #-}

module PurityNetwork where

import PurityData
import PurityUtil

import qualified Network.WebSockets as WS
import Control.Concurrent

onConnectionWrapper :: MVar ServerState -> WS.PendingConnection -> IO ()
onConnectionWrapper st pend = runConnectionT st (onConnection pend)

onConnection :: WS.PendingConnection -> ConnectionT ()
onConnection pending = do
  -- Accept every connection
  -- TODO: Make conn a monad stacked thing
  conn <- io $ WS.acceptRequest pending
  -- Fork keep alive thread for shitty clients
  io $ WS.forkPingThread conn 30
  -- Fetch the actual server state to check the game phase
  gameStage <- phase <$> grabState
  case gameStage of
    Lobby -> do
      -- Tell client that the server is in the lobby
      tellGamePhase conn
      -- Add that player to the game
      newPlayer <- addConnAsPlayer conn InLobby
      -- Give the player the list of other players, fresh from the gamestate
      tellPlayerList conn
      -- Wait for more messages
      waitForMessages (name newPlayer) (connection newPlayer)
    -- Tell them we are loading and thats it
    Loading -> tellGamePhase conn
    Playing -> do
      -- Tell Client we are in game
      tellGamePhase conn
      -- Read the rules
      r <- rules <$> grabState
      -- check if the rules say they can join mid game
      if joinMidGame r
        -- Add them to the game
        then addConnAsPlayer conn Respawning >>= \p -> waitForMessages (name p) (connection p)
        -- Give the user an error message
        else sendMessage conn "Server owner disabled joining mid game"
{-
processMessageLobby :: Player -> String -> ConnectionT ()
processMessageLobby plaName message = 
  case message of
    -- Player is readying up
    "Ready" -> 
      -- Using InLobby True
      tee
        -- Modify in Server
        (transformState . modifyPlayer player)
        -- Recurse
        waitForMessages 
        -- Set player ready
        (setReady True player)
    -- Player is unreadying up
    "Unready" -> 
      tee
        -- Modify in Server
        (transformState . modifyPlayer player)
        -- Recurse
        waitForMessages 
        -- Using InLobby False
        (setReady False player)
    "Quit" -> disconnect
    _ -> sendMessagePlayer player "Invalid Lobby Message" >> disconnect
  where
    disconnect = do
      transformState $ dropPlayer player
      mapM_ (tellPlayerList . connection) . players =<< grabState

-}
waitForMessages :: String -> WS.Connection -> ConnectionT()
waitForMessages plaName conn = do
  message <- receiveMessage conn
  -- parse the UC
  usercmd <- parseUC message
  -- Add it to the player
  transformState $ transformPlayers (\x -> if name x == plaName then addUC usercmd x else x)
  -- Recurse
  waitForMessages plaName conn

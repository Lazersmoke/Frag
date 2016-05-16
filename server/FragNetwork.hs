{-# LANGUAGE OverloadedStrings #-}

module FragNetwork where

import FragData
import FragUtil

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
      waitForMessages newPlayer
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
        then addConnAsPlayer conn Respawning >>= waitForMessages
        -- Give the user an error message
        else sendMessage conn "Server owner disabled joining mid game"

waitForMessages :: Player -> ConnectionT ()
waitForMessages pla = do
  -- Wait for a new message
  message <- receiveMessage $ connection pla
  ss <- grabState
  (case phase ss of
    Playing -> processMessageGame
    Lobby -> processMessageLobby
    Loading -> processMessageGame) pla message

processMessageLobby :: Player -> String -> ConnectionT ()
processMessageLobby player message = 
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


processMessageGame :: Player -> String -> ConnectionT()
processMessageGame pla mess = do
  -- Wait for a command
  usercmd <- parseUC mess
  tee
    -- Add to state
    (transformState . modifyPlayer pla) 
    -- Recurse
    waitForMessages 
    -- Put command on player
    (addUC usercmd pla)

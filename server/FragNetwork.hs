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
  -- Fetch the actual server state to check the game stage
  gameStage <- stage <$> grabState
  case gameStage of
    Lobby -> do
      -- Tell client that the server is in the lobby
      tellGameStage conn
      -- Add that player to the game
      newPlayer <- addConnAsPlayer conn (InLobby False)
      -- Give the player the list of other players, fresh from the gamestate
      tellPlayerList conn
      -- Wait for more messages
      waitInLobby newPlayer
    -- Tell them we are loading and thats it
    Loading -> tellGameStage conn
    Playing -> do
      -- Tell Client we are in game
      tellGameStage conn
      -- Read the rules
      r <- rules <$> grabState
      -- check if the rules say they can join mid game
      if joinMidGame r
        -- Add them to the game
        then addConnAsPlayer conn (InGame (0,0,0)) >>= waitInGame
        -- Give the user an error message
        else io . WS.sendTextData conn $ T.pack "Server owner disabled joining mid game"

waitInLobby :: Player -> ConnectionT ()
waitInLobby player = do
  grabState >>= io . print
  -- Wait for a new message
  message <- receiveMessage $ connection player
  case message of
    -- Player is readying up
    "Ready" -> 
      -- Using InLobby True
      tee
        -- Using InLobby Ready
        (setPlayerStage (InLobby True) player)
        -- Modify in Server
        (transformState . modifyPlayer player)
        -- Recurse
        waitInLobby 
    -- Player is unreadying up
    "Unready" -> 
      tee
        -- Using InLobby False
        (setPlayerStage (InLobby False) player)
        -- Modify in Server
        (transformState . modifyPlayer player)
        -- Recurse
        waitInLobby 
    "Quit" -> disconnect
  where
    disconnect = do
      transformState $ dropPlayer player
      mapM_ (tellPlayerList . connection) . players =<< grabState


waitInGame :: Player -> ConnectionT()
waitInGame pla = do
  -- Wait for a command
  usercmd <- receiveMessage (connection pla) >>= parseUC
  tee
    -- Put command on player
    (addUC usercmd pla)
    -- Add to state
    (transformState . modifyPlayer pla) 
    -- Recurse
    waitInGame 

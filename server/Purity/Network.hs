{-# LANGUAGE OverloadedStrings #-}

module Purity.Network where

import Purity.Data
import Purity.Util

import qualified Network.WebSockets as WS
import Control.Concurrent

onConnection :: MVar ServerState -> WS.PendingConnection -> IO ()
onConnection ss pending = do
  -- Accept every connection
  -- TODO: Make conn a monad stacked thing
  conn <- WS.acceptRequest pending
  -- Fork keep alive thread for shitty clients
  WS.forkPingThread conn 30
  -- Fetch the actual server state to check the game phase
  gameStage <- phase <$> readMVar ss
  case gameStage of
    Lobby -> do
      -- Tell client that the server is in the lobby
      tellGamePhase ss conn
      -- Add that player to the game
      newPlayer <- addConnAsPlayer ss conn InLobby
      -- Give the player the list of other players, fresh from the gamestate
      tellPlayerList ss conn
      -- Wait for more messages
      waitForMessages ss (name newPlayer) (connection newPlayer)
    -- Tell them we are loading and thats it
    Loading -> tellGamePhase ss conn
    Playing -> do
      -- Tell Client we are in game
      tellGamePhase ss conn
      -- Read the rules
      r <- rules <$> readMVar ss
      -- check if the rules say they can join mid game
      if joinMidGame r
        -- Add them to the game
        then addConnAsPlayer ss conn Respawning >>= \p -> waitForMessages ss (name p) (connection p)
        -- Give the user an error message
        else sendMessage conn "Server owner disabled joining mid game"

waitForMessages :: MVar ServerState -> String -> WS.Connection -> IO ()
waitForMessages ss plaName conn = do
  message <- receiveMessage conn
  -- parse the UC
  usercmd <- parseUC ss message
  -- Add it to the player
  modifyMVar_ ss $ return . transformPlayers (\x -> if name x == plaName then addUC usercmd x else x)
  -- Recurse
  waitForMessages ss plaName conn

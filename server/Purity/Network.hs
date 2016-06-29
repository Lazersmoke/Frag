{-# LANGUAGE OverloadedStrings #-}

module Purity.Network where

import Purity.Data
import Purity.Util

import qualified Network.WebSockets as WS
import Control.Concurrent

onConnection :: MVar [UserCommand] -> WS.PendingConnection -> IO ()
onConnection ucs pending = do
  -- Accept every connection
  -- TODO: Make conn a monad stacked thing
  conn <- WS.acceptRequest pending
  -- Fork keep alive thread for shitty clients
  WS.forkPingThread conn 30
  -- Ask client for their name
  sendMessage conn "What is your name?"
  -- Wait for client to give their name
  chosenName <- receiveMessage conn
  -- Add the join command to the user commands
  addUC ucs (mkUserCommand "join" chosenName)
  -- Wait for additional commands TODO: change chosenName to an actual, stateful identifier
  waitForMessages ucs chosenName conn

addUC :: MVar [UserCommand] -> UserCommand -> IO ()
addUC ucs uc = modifyMVar_ ucs $ return . (uc:)

waitForMessages :: MVar [UserCommand] -> Identifier -> WS.Connection -> IO ()
waitForMessages ucs pIdent conn = do
  -- recieve a message, wrap it in a UC, and add it to the list
  addUC ucs . (flip mkUserCommand) pIdent =<< receiveMessage conn 
  -- Recurse
  waitForMessages ucs pIdent conn

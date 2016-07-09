{-# LANGUAGE OverloadedStrings #-}

module Purity.Network where

import Purity.Data
import Purity.Util

import Data.Access
import qualified Network.WebSockets as WS
import Control.Concurrent

import Debug.Trace

onConnection :: MVar [Command] -> WS.PendingConnection -> IO ()
onConnection cmds pending = do
  -- Accept every connection
  -- TODO: Make conn a monad stacked thing
  conn <- WS.acceptRequest pending
  -- Fork keep alive thread for shitty clients
  WS.forkPingThread conn 30
  -- Ask client for their name
  sendMessage conn "What is your name?"
  -- Wait for client to give their name
  chosenName <- receiveMessage conn
  -- Add the Join command to the user commands
  addCommand cmds (mkCommand ("Join " ++ chosenName) chosenName Client)
  -- Fork thread for sending packets to the client TODO: Manage the thread instead of throwing it out
  _ <- forkIO $ sendMessages cmds chosenName conn
  -- Wait for additional commands TODO: change chosenName to an actual, stateful identifier
  waitForMessages cmds chosenName conn

addCommand :: MVar [Command] -> Command -> IO ()
addCommand cmds cmd = modifyMVar_ cmds $ return . (cmd:)

sendMessages :: MVar [Command] -> Identifier -> WS.Connection -> IO ()
sendMessages cmds pIdent conn = do
  -- Send all the matching messages' command statements to the client
  mapM_ (sendMessage conn) =<< map (grab command) . filter matches <$> readMVar cmds
  -- Clear out the messages we sent
  pModifyMVar_ cmds $ filter (not . matches)
  threadDelay 1000000
  sendMessages cmds pIdent conn
  where
    matches c = (Server == source ~>> c) && (pIdent == cmdId ~>> c)


waitForMessages :: MVar [Command] -> Identifier -> WS.Connection -> IO ()
waitForMessages ucs pIdent conn = do
  -- recieve a message, wrap it in a UC, and add it to the list
  addCommand ucs . (\n -> mkCommand n pIdent Client) =<< receiveMessage conn 
  -- Recurse
  waitForMessages ucs pIdent conn

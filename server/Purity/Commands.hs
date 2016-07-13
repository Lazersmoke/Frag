{-# LANGUAGE TupleSections #-}
module Purity.Commands where

import Purity.Data
import Purity.Util

import Data.Access

performCommand :: Command -> Homomorphism ServerState
performCommand cmd = case firstWord $ command ~>> cmd of
  -- cmdObject :: Command -> ServerState ~> Object
  "+forward" -> cmdObject cmd >&> wish >&> vecZ >@> 1
  "-forward" -> cmdObject cmd >&> wish >&> vecZ >@> 0

  "+back" -> cmdObject cmd >&> wish >&> vecZ >@> (-1)
  "-back" -> cmdObject cmd >&> wish >&> vecZ >@> 0

  "+right" -> cmdObject cmd >&> wish >&> vecX >@> 1
  "-right" -> cmdObject cmd >&> wish >&> vecX >@> 0

  "+left" -> cmdObject cmd >&> wish >&> vecX >@> (-1)
  "-left" -> cmdObject cmd >&> wish >&> vecX >@> 0

  "+jump" -> cmdObject cmd >&> ((mode >@> InAir) . (wish >&> vecY >@> 1))
  "-jump" -> cmdObject cmd >&> wish >&> vecY >@> 0

  -- "look dx dy" 
  "look" -> playerLook cmd
  _ -> error ("Invalid Command: " ++ (command ~>> cmd))

performLobbyCommand :: Command -> Homomorphism ServerState
performLobbyCommand cmd = case firstWord $ command ~>> cmd of
  "Join" -> addPlayer cmd
  "Ready" -> specificPlayer (cmdId ~>> cmd) >&> ready >@> True
  "Unready" -> specificPlayer (cmdId ~>> cmd) >&> ready >@> False
  _ -> error ("Invalid Lobby Command: " ++ (command ~>> cmd))

-- Safely get the first word, "" if no words
firstWord :: String -> String
firstWord [] = []
firstWord str = head . words $ str

type Homomorphism a = a -> a

playerLook :: Command -> Homomorphism ServerState
playerLook cmd = case map maybeRead delta of
  -- If the deltas were read ok, then transform the object
  [Just dx, Just dy] -> 
    cmdObject cmd >&> ((yaw >&> (+ sens * dx)) . (pitch >&> (+ sens * dy))) -- Add mouse delta to pitch
  -- If the deltas are invalid then don't do anything
  _ -> id 
  where
    delta = drop 1 $ words (command ~>> cmd)
    sens = 0.01

addPlayer :: Command -> Homomorphism ServerState
addPlayer cmd ss = 
  -- Validate the chosen name and switch over it
  ($ss) $ switch
    -- If valid, add it to the state
    (players >&> (newPlayer:)) 
    -- If Invalid, return unmodified ss
    id
    -- Is it valid? (length < 50 and not already chosen) (switch on this)
    ((length chosenName < 50 &&) . notElem chosenName . map (name ~>>) . grab players $ ss)
    where
      chosenName = cmdId ~>> cmd
      newPlayer = Player -- Make a new player
        chosenName -- With the chosen name (as ident)
        chosenName -- With the chosen name
        Lost
        (size >@> scale 0.1 unitVector $ emptyObject)
        False -- Not Ready

performCommands :: [Command] -> Homomorphism ServerState
-- No commands means do nothing
performCommands [] = id 
-- some commands means do the first one, then do the rest
performCommands (x:y) = performCommands y . performCommand x

performLobbyCommands :: [Command] -> Homomorphism ServerState
-- No commands means do nothing
performLobbyCommands [] = id 
-- some commands means do the first one, then do the rest
performLobbyCommands (x:y) = performLobbyCommands y . performLobbyCommand x


-- do all the user commands in s, then set the Command list to []
-- Legacy code to manage ServerState ~> userCmds
--doUserCmds :: Homomorphism ServerState
--doUserCmds s = (userCmds >@> []) . performCommands (userCmds ~>> s) $ s

-- +forward command
-- Set the player's wish to 0 0 1

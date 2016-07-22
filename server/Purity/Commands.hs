{-# LANGUAGE TupleSections #-}
module Purity.Commands where

import Purity.Data
import Purity.Util

import Data.Access

performCommand :: Command -> Homomorphism ServerState
performCommand cmd = case firstWord $ CommandText ~>> cmd of
  -- cmdObject :: Command -> ServerState ~> Object
  "+forward" -> CmdObject cmd >&> Wish >&> Z >@> 1
  "-forward" -> CmdObject cmd >&> Wish >&> Z >@> 0

  "+back" -> CmdObject cmd >&> Wish >&> Z >@> (-1)
  "-back" -> CmdObject cmd >&> Wish >&> Z >@> 0

  "+right" -> CmdObject cmd >&> Wish >&> X >@> 1
  "-right" -> CmdObject cmd >&> Wish >&> X >@> 0

  "+left" -> CmdObject cmd >&> Wish >&> X >@> (-1)
  "-left" -> CmdObject cmd >&> Wish >&> X >@> 0

  "+jump" -> CmdObject cmd >&> ((Status >@> InAir) . (Wish >&> Y >@> 1))
  "-jump" -> CmdObject cmd >&> Wish >&> Y >@> 0

  -- "look dx dy" 
  "look" -> playerLook cmd
  _ -> error ("Invalid Command: " ++ (CommandText ~>> cmd))

performLobbyCommand :: Command -> Homomorphism ServerState
performLobbyCommand cmd = case firstWord $ CommandText ~>> cmd of
  "Join" -> addPlayer cmd
  "Ready" -> Specific (Identity ~>> cmd) >&> Ready >@> True
  "Unready" -> Specific (Identity ~>> cmd) >&> Ready >@> False
  _ -> error ("Invalid Lobby Command: " ++ (CommandText ~>> cmd))

-- Safely get the first word, "" if no words
firstWord :: String -> String
firstWord [] = []
firstWord str = head . words $ str

type Homomorphism a = a -> a

playerLook :: Command -> Homomorphism ServerState
playerLook cmd = case map maybeRead delta of
  -- If the deltas were read ok, then transform the object
  [Just dx, Just dy] -> 
    CmdObject cmd >&> ((yaw >&> (+ sens * dx)) . (pitch >&> (min 1.57 . max (-1.57) . subtract (sens * dy)))) -- Add mouse delta to pitch
  -- If the deltas are invalid then don't do anything
  _ -> id 
  where
    delta = drop 1 $ words (CommandText ~>> cmd)
    sens = 0.01

addPlayer :: Command -> Homomorphism ServerState
addPlayer cmd ss = 
  -- Validate the chosen name and switch over it
  ($ss) $ switch
    -- If valid, add it to the state
    (Players >&> (newPlayer:)) 
    -- If Invalid, return unmodified ss
    id
    -- Is it valid? (length < 50 and not already chosen) (switch on this)
    ((length chosenName < 50 &&) . notElem chosenName . map (Name ~>>) . grab Players $ ss)
    where
      chosenName = Identity ~>> cmd
      newPlayer = Player -- Make a new player
        chosenName -- With the chosen name (as ident)
        chosenName -- With the chosen name
        Lost
        (Size >@> scale 0.1 unitVector $ emptyObject)
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

{-# LANGUAGE TupleSections #-}
module Purity.Commands where

import Purity.Data
import Purity.Util

import Data.Access

performUC :: UserCommand -> ServerState -> ServerState
performUC uc = case firstWord $ command ~>> uc of
  "+forward" -> ucObject uc >&> wish >&> vecZ >@> 1
  "-forward" -> ucObject uc >&> wish >&> vecZ >@> 0

  "+back" -> ucObject uc >&> wish >&> vecZ >@> (-1)
  "-back" -> ucObject uc >&> wish >&> vecZ >@> 0

  "+right" -> ucObject uc >&> wish >&> vecX >@> 1
  "-right" -> ucObject uc >&> wish >&> vecX >@> 0

  "+left" -> ucObject uc >&> wish >&> vecX >@> (-1)
  "-left" -> ucObject uc >&> wish >&> vecX >@> 0

  "+jump" -> ucObject uc >&> ((mode >@> InAir) . (wish >&> vecY >@> 1))
  "-jump" -> ucObject uc >&> wish >&> vecY >@> 0

  -- "look dx dy" 
  "look" -> playerLook uc
  _ -> id

firstWord :: String -> String
firstWord [] = []
firstWord str = head . words $ str

type UCAction = ServerState -> ServerState

playerLook :: UserCommand -> UCAction
playerLook uc = case map maybeRead delta of
  -- If the deltas were read ok, then transform the object
  [Just dx, Just dy] -> 
    ucObject uc >&> ((yaw >&> (+ sens * dx)) . (pitch >&> (+ sens * dy))) -- Add mouse delta to pitch
  -- If the deltas are invalid then don't do anything
  _ -> id 
  where
    delta = drop 1 $ words (command ~>> uc)
    sens = 0.01

addPlayer :: UserCommand -> ServerState -> ServerState 
addPlayer uc ss = 
  -- Validate the chosen name and switch over it
  switch
    -- If valid, add it to the state
    (players >&> (newPlayer:) $ ss) 
    -- If Invalid, return unmodified ss
    ss
    -- Is it valid? (length < 50 and not already chosen) (switch on this)
    (length chosenName < 50 && (notElem chosenName . map (name ~>>) $ players ~>> ss))
    where
      chosenName = playerIdentity ~>> uc
      newPlayer = Player -- Make a new player
        chosenName -- With the chosen name (as ident)
        chosenName -- With the chosen name
        Lost
        (size >@> scale 0.1 unitVector $ emptyObject)
        False -- Not Ready

noAct :: Player -> UCAction
noAct = const id

performUCs :: [UserCommand] -> ServerState -> ServerState
performUCs [] = id 
performUCs (x:y) = performUCs y . performUC x

-- For each player, get and S -> S, then put them all in a list, concat, and apFold
doUserCmds :: ServerState -> ServerState
doUserCmds s = (userCmds >@> []) . performUCs (userCmds ~>> s) $ s

-- +forward command
-- Set the player's wish to 0 0 1

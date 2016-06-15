{-# LANGUAGE TupleSections #-}
module Purity.Commands where

import Purity.Data
import Purity.Util

performUC :: Player -> UserCommand -> ServerState -> ServerState
performUC p uc = ($ p) $ case firstWord $ command ~>> uc of
  "+forward" -> playerMove (vecZ >@> 1)
  "-forward" -> playerMove (vecZ >@> 0)

  "+back" -> playerMove (vecZ >@> (-1))
  "-back" -> playerMove (vecZ >@> 0)

  "+right" -> playerMove (vecX >@> 1)
  "-right" -> playerMove (vecX >@> 0)

  "+left" -> playerMove (vecX >@> (-1))
  "-left" -> playerMove (vecX >@> 0)

  "+jump" -> playerJump
  "-jump" -> playerUnJump

  -- "look dx dy" 
  "look" -> playerLook (command ~>> uc)
  _ -> noAct

firstWord :: String -> String
firstWord [] = []
firstWord str = head . words $ str

type UCAction = Player -> ServerState -> ServerState
-- transform the object by transforming the wish
playerMove :: (Vector -> Vector) -> UCAction
playerMove v = transformPlayersObject (wish >&> v) 

playerJump :: UCAction
playerJump = transformPlayersObject (set mode InAir . change wish (vecY >@> 1))

playerUnJump :: UCAction
playerUnJump = transformPlayersObject (wish >&> (vecY >@> 0))

playerLook :: String -> UCAction
playerLook cmd = case map maybeRead delta of
  -- If the deltas were read ok, then transform the object
  [Just dx, Just dy] -> 
    transformPlayersObject (
      (yaw >&> (+ sens * dx))-- Add mouse delta to yaw
      . (pitch >&> (+ sens * dy)) -- Add mouse delta to pitch
    )
  -- If the deltas are invalid then don't do anything
  _ -> noAct
  where
    delta = drop 1 $ words cmd
    sens = 0.01

transformPlayersObject :: (Object -> Object) -> UCAction
transformPlayersObject f p = changeMap players (\x -> if x == p then (object >&> f) x else x)

noAct :: UCAction
noAct = const id

performUCs :: Player -> [UserCommand] -> ServerState -> ServerState
performUCs _ [] = id 
performUCs p (x:y) = performUCs p y . performUC p x

-- For each player, get and S -> S, then put them all in a list, concat, and apFold
doUserCmds :: ServerState -> ServerState
doUserCmds s = emptyCmds . (listApFold . map (\p -> performUCs p (userCmds ~>> p)) $ players ~>> s) $ s
  where
    emptyCmds = changeMap players (set userCmds []) 

-- +forward command
-- Set the player's wish to 0 0 1

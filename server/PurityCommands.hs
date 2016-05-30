{-# LANGUAGE TupleSections #-}
module PurityCommands where

import PurityData
import PurityUtil

performUC :: Player -> UserCommand -> ServerState -> ServerState
performUC p uc = ($ p) $ case firstWord $ command uc of
  "+forward" -> playerMove (setVecZ 1)
  "-forward" -> playerMove (setVecZ 0)

  "+back" -> playerMove (setVecZ (-1))
  "-back" -> playerMove (setVecZ 0)

  "+right" -> playerMove (setVecX 1)
  "-right" -> playerMove (setVecX 0)

  "+left" -> playerMove (setVecX (-1))
  "-left" -> playerMove (setVecX 0)

  -- "look dx dy" 
  "look" -> playerLook (command uc)
  _ -> noAct

firstWord :: String -> String
firstWord [] = []
firstWord str = head . words $ str

type UCAction = Player -> ServerState -> ServerState
-- transform the object by transforming the wish
playerMove :: (Vector -> Vector) -> UCAction
playerMove v = transformPlayersObject (transformWish v) 

playerLook :: String -> UCAction
playerLook cmd = case map maybeRead delta of
  -- If the deltas were read ok, then transform the object
  [Just dx, Just dy] -> 
    transformPlayersObject (\o -> 
      o{
      -- Add mouse delta to yaw
      yaw = yaw o + sens * dx,
      -- Add mouse delta to pitch
      pitch = pitch o + sens * dy
      }
    )
  -- If the deltas are invalid then don't do anything
  _ -> noAct
  where
    delta = drop 1 $ words cmd
    sens = 0.01

transformPlayersObject :: (Object -> Object) -> UCAction
transformPlayersObject f p = transformPlayers (\x -> if x == p then transformObject f x else x)

noAct :: UCAction
noAct = const id

performUCs :: Player -> [UserCommand] -> ServerState -> ServerState
performUCs _ [] = id 
performUCs p (x:y) = performUCs p y . performUC p x

-- For each player, get and S -> S, then put them all in a list, concat, and apFold
doUserCmds :: ServerState -> ServerState
doUserCmds s = emptyCmds . (listApFold . map (\p -> performUCs p (userCmds p)) $ players s) $ s
  where
    emptyCmds = transformPlayers (\p -> p {userCmds = []}) 

-- +forward command
-- Set the player's wish to 0 0 1

{-# LANGUAGE TupleSections #-}
module FragCommands where

import FragData
import FragUtil
import Debug.Trace

performUC :: Player -> UserCommand -> ServerState -> ServerState
performUC p uc = ($ p) $ case traceShowId (command uc) of
  "+forward" -> playerMove (setVecZ 1)
  "-forward" -> playerMove (setVecZ 0)

  "+back" -> playerMove (setVecZ (-1))
  "-back" -> playerMove (setVecZ 0)

  "+right" -> playerMove (setVecX 1)
  "-right" -> playerMove (setVecX 0)

  "+left" -> playerMove (setVecX (-1))
  "-left" -> playerMove (setVecX 0)
  _ -> noAct

type UCAction = Player -> ServerState -> ServerState
-- transform the object by transforming the wish
playerMove :: (Vector -> Vector) -> UCAction
playerMove v p = transformPlayers (\x -> if x == p then transformObject (transformWish v) x else x)

noAct :: UCAction
noAct _ = id

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

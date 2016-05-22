{-# LANGUAGE TupleSections #-}
module FragCommands where

import FragData
import Debug.Trace

performUC :: UserCommand -> Player -> Player
performUC uc = case traceShowId (command uc) of
  "+forward" -> playerMove (setVecZ 1)
  "-forward" -> playerMove (setVecZ 0)

  "+back" -> playerMove (setVecZ (-1))
  "-back" -> playerMove (setVecZ 0)

  "+right" -> playerMove (setVecX 1)
  "-right" -> playerMove (setVecX 0)

  "+left" -> playerMove (setVecX (-1))
  "-left" -> playerMove (setVecX 0)
  _ -> noAct

type UCAction = Player -> Player
-- transform the object by transforming the wish
playerMove :: (Vector -> Vector) -> UCAction
playerMove = transformObject . transformWish

noAct :: UCAction
noAct = id

-- Apply a list of homomorphisms in order
listApFold :: [a -> a] -> a -> a
listApFold list orig = foldl (flip id) orig list

-- For each player, get and S -> S, then put them all in a list, concat, and apFold
doUserCmds :: ServerState -> ServerState
doUserCmds s = s {players = map (\p -> foldl (flip performUC) p {userCmds = []} (userCmds p)) (players s)}

{-# LANGUAGE TupleSections #-}
module PurityCommands where

import PurityData
import PurityUtil
import Data.Maybe

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

-- transform the object by transforming the wish
playerLook :: String -> UCAction
playerLook command = case map maybeRead delta of
  [Just dx, Just dy] -> transformPlayersObject (\o -> o {dir = Vector (0, ((0.1 * dy) +) $ vecY (dir o),1)})
  -- Const drops the player arg; Invalid delta means no movement for anyone
  _ -> const id
  where
    delta = drop 1 $ words command

-- sin( pitch ) = y coord
getPitch :: Vector -> VectorComp
getPitch v = asin $ vecY nor
  where
    nor = normalizeVector v

getYaw :: Vector -> VectorComp
getYaw v = asin . (/cos (getPitch v)) $ vecX nor
  where
    nor = normalizeVector v

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

parseVector :: String -> Maybe Vector
parseVector str =
    case map maybeRead w of
      [Just x, Just y, Just z] -> Just $ Vector (x, y, z)
      _ -> Nothing
  where
    w = words str


transformPlayersObject :: (Object -> Object) -> UCAction
transformPlayersObject f p = transformPlayers (\x -> if x == p then transformObject f x else x)

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

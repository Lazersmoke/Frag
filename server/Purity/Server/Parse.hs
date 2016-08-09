import Purity.Server.Data
import Purity.Server.Util

import Data.List.Split
-- 0.1 0 0|5 5 5|0 0 0
type ParseError = String
type ErrorProne a = Either ParseError a

parseWP :: String -> ErrorProne WorldPlane
parseWP input = 
  case map parseVector . splitOn "|" $ input of
    [Right a, Right b, Right c] -> Right $ WorldPlane (a,b,c) 
    _ -> Left input

parseVector :: String -> ErrorProne Vector
parseVector input = 
  case map maybeRead . words $ input of
    [Just x, Just y, Just z] -> Right $ Vector (x,y,z)
    _ -> Left input

listToWP :: [Vector] -> Maybe WorldPlane
listToWP [a,b,c] = Just $ WorldPlane (a,b,c)
listToWP _ = Nothing

listToVector :: [VectorComp] -> Maybe Vector
listToVector [x,y,z] = Just $ Vector (x,y,z)
listToVector _ = Nothing


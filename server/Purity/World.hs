module Purity.World where

import Purity.Data

import Data.Access
import Text.Parsec
import Text.Parsec.String

testObject :: Object
testObject = (Wish >@> Vector (1,0,0)) . (Velocity >@> Vector (-1,-1,-1)) $ oneCube

getServerState :: String -> IO ServerState
getServerState fn = do
  wpl <- parseFromFile parseWPL fn
  case wpl of
    Right geometry -> do
      print geometry 
      return $ (AccessWorld ~> Geometry >@> geometry) . (AccessWorld ~> Name >@> fn) . (ObjectA >&> (testObject:)) $ freshServerState
    Left err -> do
      print err 
      return freshServerState

parseWPL :: Parser [WorldPlane]
parseWPL = do
  wpl <- worldPlane `sepBy` endOfLine
  eof
  return wpl

worldPlane :: Parser WorldPlane 
worldPlane = do
  [x,y,z] <- count 3 vector 
  return $ WorldPlane (x,y,z)

vector :: Parser Vector
vector = do
  [x,y,z] <- count 3 num
  return $ Vector (x,y,z)

num :: Parser Double
num = do
  num <- many1 digit
  optional space
  return $ (read :: String -> Double) num

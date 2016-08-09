module Purity.Server.World where

import Purity.Server.Data

import Data.Access
import Control.Monad
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
  wpl <- worldPlane `endBy` endOfLine
  eof
  return wpl

comment :: Parser () 
comment = void $ between (char ':') (char ';') (many (alphaNum <|> oneOf "+-/*" <|> space)) 

worldPlane :: Parser WorldPlane 
worldPlane = do
  [x,y,z] <- count 3 vector 
  optional comment
  return $ WorldPlane (x,y,z)

vector :: Parser Vector
vector = do
  void $ char '<'
  x <- number
  void $ char ','
  y <- number
  void $ char ','
  z <- number
  void $ char '>'
  return $ Vector (x,y,z)

number :: Parser Double
number = do
  sign <- anyChar
  num <- many1 digit
  return . (if sign == '-' then negate else id) . (read :: String -> Double) $ num

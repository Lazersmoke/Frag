{-# LANGUAGE OverloadedStrings #-}
module Purity.ParseOBJ where

import qualified Numeric.LinearAlgebra as Mat
import Data.Attoparsec.ByteString.Char8
import Data.Int (Int16)

parseVertex :: Parser [Float]
parseVertex = char 'v' *> sequence [parseCoord,parseCoord,parseCoord,option 1 parseCoord]

parseTextureCoord :: Parser [Float]
parseTextureCoord = string "vt" *> sequence [parseCoord,parseCoord,option 1 parseCoord]

parseNormalCoord :: Parser [Float]
parseNormalCoord = string "vn" *> sequence [parseCoord,parseCoord,parseCoord]

parseFace :: Parser [(Int,Int,Int)]
parseFace = char 'f' *> count 3 (space *> parseSlashes)

parseSlashes :: Parser (Int,Int,Int)
parseSlashes = (,,) <$> (decimal <* char '/') <*> decimal <*> (char '/' *> decimal)

parseCoord :: Parser Float
parseCoord = space *> (realToFrac <$> double)

parseCommentLine :: Parser ()
parseCommentLine = char '#' *> skipWhile (\c -> c /= '\r' && c /= '\n') *> endOfLine

parseBlankLine :: Parser ()
parseBlankLine = many' space *> skipWhile (\c -> c /= '\r' && c /= '\n') *> endOfLine

arrange :: [(a,b,c)] -> ([a],[b],[c])
arrange ((a,b,c):xs) = (a:getFirst rest,b:getSecond rest,c:getThird rest)
  where
    getFirst (x,_,_) = x
    getSecond (_,x,_) = x
    getThird (_,_,x) = x
    rest = arrange xs

parseFile :: Parser (Mat.Vector Float,Mat.Vector Float,Mat.Vector Float)
parseFile = do
  vs <- parseLinesOf parseVertex
  ts <- parseLinesOf parseTextureCoord
  ns <- parseLinesOf parseNormalCoord

parseLinesOf :: Parser a -> Parser [a]
parseLinesOf p = concat <$> many' (choice [parseCommentLine *> pure [], parseBlankLine *> pure [],(:[]) <$> p])

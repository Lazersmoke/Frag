module Purity.World where

import Purity.Data
import Purity.Util
import Purity.Parse

import Data.List.Split

worldPlanesFromFile :: String -> IO [WorldPlane]
worldPlanesFromFile fn = readFile fn >>= parseWPL

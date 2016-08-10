module Purity.Client.Util where

import Purity.Client.Data

plog :: LogLevel -> String -> IO ()
plog level message = case level of
  Log -> putStrLn $ "[Purity] " ++ message
  Warn -> putStrLn $ "[Purity][Warning] " ++ message
  Error -> putStrLn $ "[Purity][ERROR] " ++ message

module Purity.Client.Util where

plog :: String -> IO ()
plog message = putStrLn $ "[Purity] " ++ message

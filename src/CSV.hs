module CSV where

import Data.List.Split

readDataFile :: [Char] -> IO String
readDataFile filename = readFile $ "data/CTA_Round1/" ++ filename ++ ".csv"

fstColumnValues :: String -> [String]
fstColumnValues file =  fmap (!! 0) . fmap (splitOn ",") $ lines file 

dequote :: String -> String
dequote = filter (/='\"')

fstLineValues :: String -> [String]
fstLineValues = fmap (dequote) . splitOn "," . head . lines


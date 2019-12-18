module CSV where

import Data.List.Split

readDataFile :: [Char] -> IO String
readDataFile filename = readFile $ "data/CTA_Round1/" ++ filename ++ ".csv"

columnValues :: Int -> String -> [String]
columnValues index file =  fmap (!! index) . fmap (splitOn ",") $ lines file 

dequote :: String -> String
dequote = filter (/='\"')

lineValues :: Int -> String -> [String]
lineValues index text = fmap (dequote) . splitOn "," $ (lines text) !! index  


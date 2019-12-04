module Main where

import CSV 
import SparqlClient

import Data.List

main :: IO ()
main = do
    targetFileContent <- readFile "data/CTA_Round1_Targets.csv"
    let fileNames = sort . filter (not . null) $ fstColumnValues targetFileContent 
    dataFiles <- mapM readDataFile fileNames
    putStrLn . concat . intersperse "\n" $ fmap (unwords) . fmap (fstLineValues) $ dataFiles
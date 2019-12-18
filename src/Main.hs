module Main where

import CSV 
import SparqlClient

import Data.List
import Text.Read
import Data.Maybe

import Data.Char

main :: IO ()
main = do
    targetFileContent <- readFile "data/CTA_Round1_Targets.csv"
    let fileNames = sort . filter (not . null) $ columnValues 0 targetFileContent 
    dataFiles <- mapM readDataFile fileNames
    putStrLn . concat . intersperse "\n" $ fmap (unwords) . fmap (lineValues 0) $ dataFiles

getTargets :: IO [(String, Int)]
getTargets = do
    targetFileContent <- readFile "data/CTA_Round1_Targets.csv"
    let
        filenames = columnValues 0 targetFileContent 
        columns   = fmap (readMaybe) $ columnValues 1 targetFileContent :: [Maybe Int]
        result = fmap (\(a, b) -> (a, fromJust b)) 
            . filter ((/=) Nothing . snd)
            $ zip filenames columns
    return result

processTarget :: (String, Int) -> IO ()
processTarget (filename, column) = do
    tableContent <- readDataFile filename
    let
        colName = formatColumnName $ (lineValues 0 tableContent) !! column
        colVals = columnValues column tableContent 
    return ()

formatColumnName :: String -> String
formatColumnName name = 
    fmap toLower 
    . fmap (\a -> if a==' ' then '_' else a)
    . trim
    . fst
    $ break (`elem` ['(', '?', '[']) name

processColumnName :: String -> IO [String]
processColumnName colName = do
    return colName
    >>= getSuperClasses

owlBaseClasses :: [String]
owlBaseClasses = ["owl", "http"]

topClassesCount = 5
topDescriptionClassesCount = 3

processColumnValues :: [String] -> IO [String]
processColumnValues colVals = do 
    lookupResult  <- lookupElems colVals
    superClasses  <- getSuperClassesForFrequencyList topClassesCount . getFrequencyList $ processClasses lookupResult 
    superClasses' <- getSuperClassesForFrequencyList topDescriptionClassesCount . getFrequencyList $ processDescriptions lookupResult
    return $ case superClasses of
        [] -> case superClasses' of
            [] -> ["no_class"]
            ok -> ok
        ok -> ok

getSuperClassesForFrequencyList :: Int -> [(String, Int)] -> IO [String]
getSuperClassesForFrequencyList limit freqList = do
    fmap (nub . concat)
    . mapM getSuperClasses
    . fmap formatName
    . take limit
    . filter (not . isPrefixOfAny)
    . fmap fst
    $ freqList
    where 
        formatName = concat . fmap fstLetterUpper . words
        fstLetterUpper [] = []
        fstLetterUpper (x:xs) = (toUpper x : xs)
        isPrefixOfAny a = any (==True) $ fmap (`isPrefixOf` a) owlBaseClasses
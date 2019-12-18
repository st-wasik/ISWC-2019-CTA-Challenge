module Main where

import CSV 
import SparqlClient

import Data.List
import Text.Read
import Data.Maybe

import Data.Char
import Text.Pretty.Simple

main :: IO ()
main = do
    targetFileContent <- readFile "data/CTA_Round1_Targets.csv"
    let fileNames = sort . filter (not . null) $ columnValues 0 targetFileContent 
    dataFiles <- mapM readDataFile fileNames
    putStrLn . concat . intersperse "\n" $ fmap (unwords) . fmap (lineValues 0) $ dataFiles

process :: IO [[String]]
process = do
    getTargets
    >>= (\a-> return $ take 7 a)
    >>= mapM processTarget

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

columnValsLimit = 10

processTarget :: (String, Int) -> IO [String]
processTarget (filename, column) = do
    tableContent <- readDataFile filename
    let
        allVals = columnValues column tableContent 
        colVals = take columnValsLimit $ drop 1 allVals
        colName = head allVals
    columnResult <- processColumnName colName
    descriptionResult <- processColumnValues colVals
    putStr $ "COLUMN: " ++ colName ++ "   VALUES: "
    mapM_ (\a -> putStr $ a ++ "; ") colVals 
    putStrLn ""
    return $ if not . null $ columnResult 
        then columnResult
        else descriptionResult
    >>= (\a-> pPrint a >> return a)


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
        isPrefixOfAny a = any (==True) $ fmap (`isPrefixOf` a) owlBaseClasses
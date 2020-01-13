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

 -- | Begins processing targets. 
--process :: IO [(String, Int, String)]
process = do
    getTargets
    >>= (\a-> return $ {-take 5 $ drop 5 $-} a)
    >>= mapM processTarget
    >>= mapM (return . (\(a, b, c) -> show a ++ ", " ++ (show $ show b) ++ ", " ++ concat (intersperse " | " (fmap show c))))
    >>= return . unlines 
    >>= writeFile "CTA-Round1-result.csv"

 -- | Returns list of targets read from target file. One target is pair: table name & column no.
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

-- | Reads target column name & content. Sends query with column name to dbpedia. 
--   If result is null, sends queries for first X values of column to dbpedia. 
--   Shows resulting classes and superclasses on std out.
processTarget :: (String, Int) -> IO (String, Int, [String])
processTarget (filename, column) = do
    tableContent <- readDataFile filename
    let
        allVals = columnValues column tableContent 
        colVals = take columnValsLimit $ drop 1 allVals
        colName = head allVals

    columnResult <- processColumnName colName
    descriptionResult <- processColumnValues colVals

    putStr $ "COLUMN: " ++ colName ++ "\nVALUES: "
    mapM_ (\a -> putStr $ a ++ "; ") colVals 
    putStrLn ""
    return $ if not . null $ columnResult 
        then columnResult
        else descriptionResult
    >>= return . filter (/="http://dbpedia.org/ontology/Agent") . filter (/="http://dbpedia.org/ontology/Thing")
    >>= \a-> (putStrLn $ "RESULT for " ++ filename ++ " " ++ (show column) ++ ": ") 
    >> pPrint a 
    >> return (filename, column, a)

processColumnName :: String -> IO [String]
processColumnName colName = do
    return colName
    >>= getSuperClasses
    >>= (\classes-> case classes of
        [] -> return []
        ok -> return $ ok)
        
lookupColName colName = do    
    lookupResult  <- lookupElems [colName]
    getSuperClassesForFrequencyList lookupResult topClassesCount . getFrequencyList $ processClasses lookupResult 
    

owlBaseClasses :: [String]
owlBaseClasses = ["owl", "http"]

topClassesCount = 5
topDescriptionClassesCount = 3

processColumnValues :: [String] -> IO [String]
processColumnValues colVals = do 
    lookupResult  <- lookupElems colVals
    superClasses  <- getSuperClassesForFrequencyList lookupResult topClassesCount . getFrequencyList $ processClasses lookupResult 
    superClasses' <- getSuperClassesForFrequencyList lookupResult topDescriptionClassesCount . getFrequencyList $ processDescriptions lookupResult
    return $ case superClasses of
        [] -> case superClasses' of
            [] -> ["no_class"]
            ok -> ok
        ok -> ok

getSuperClassesForFrequencyList :: [LookupResult] -> Int -> [(String, Int)] -> IO [String]
getSuperClassesForFrequencyList lookupData limit freqList = do
    fmap nub
    . fmap ((mapToDBPediaClasses baseClasses) ++)
    . fmap (concat)
    . mapM getSuperClasses
    . fmap (formatName False)
    $ baseClasses
    where 
        isPrefixOfAny a = any (==True) $ fmap (`isPrefixOf` a) owlBaseClasses
        baseClasses = 
            take limit
            . filter (not . isPrefixOfAny)
            . fmap fst
            $ freqList 
        mapToDBPediaClasses values = filter (not . isPrefixOf "http://schema.org/") 
            $ foldl (\acc (LookupResult _ _ a _) -> acc ++ (foldlClasses a)) [] lookupData
            where foldlClasses = foldl (\acc a -> if (fst a) `elem` values then (snd a):acc else acc ) []
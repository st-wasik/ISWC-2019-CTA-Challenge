module SparqlClient where

import qualified Database.SPARQL.Protocol.Client    as SPARQL
import qualified Data.ByteString.Lazy.Char8         as BSC8
import Text.Pretty.Simple -- pPrint

import Control.Exception
import Network.HTTP.Conduit
import Network.HTTP.Client
import qualified Network.HTTP as HTTP

import Data.List.Split
import Data.List

import Data.Char

type SelectResult = SPARQL.SelectResult


type Description = String
type Classes = [(String, String)]
type Categories = [(String, String)]
type Name = String

data LookupResult = LookupResult Name Description Classes Categories deriving (Eq, Show, Read)

trim :: String -> String
trim x = let y = dropWhile (==' ') x in reverse $ dropWhile (==' ') (reverse y)

dbpediaEndpoint :: String
dbpediaEndpoint = "http://dbpedia.org/sparql"

selectQuery :: String
selectQuery = "select distinct ?Concept where {[] a ?Concept} LIMIT 100"

sendExampleSelect :: IO (Response SelectResult)
sendExampleSelect = sendQuery dbpediaEndpoint selectQuery

sendQuery :: String -> String -> IO (Response SelectResult)
sendQuery endpoint query = SPARQL.select endpoint (BSC8.pack query) 

sendEither :: IO (Response SelectResult) -> IO (Either HttpException (Response SelectResult))
sendEither request = try request



baseLookupEndpoint :: String
baseLookupEndpoint = "http://lookup.dbpedia.org/api/search/KeywordSearch?MaxHits=1&QueryString="

xmlTags :: [String]
xmlTags = ["Classes", "Class", "Label", "URI", "Categories", "Category"]

dropXmlTagsAndZipUnique :: [String] -> [(String, String)]
dropXmlTagsAndZipUnique [] = []
dropXmlTagsAndZipUnique (a:_) = 
    nubBy (\a b -> fst a == fst b) 
    . foldl (\acc a -> 
        case a of
            [a,b] -> ((a,b):acc)
            _     -> acc) [] 
    . chunksOf 2 
    . filter (`notElem` xmlTags) 
    . filter (not . isPrefixOf "/") 
    . filter (not . null) 
    . concat 
    . fmap (splitOn ">") 
    $ splitOn "<" a

lookupForName :: String -> IO LookupResult
lookupForName name = do
    res <- (HTTP.simpleHTTP . HTTP.getRequest $ baseLookupEndpoint ++ name) >>= HTTP.getResponseBody 
    let 
        description = getDescriptionFromLookup res
        classes     = getClassesFromLookup res
        categories  = getCategoriesForLookup res
    return $ LookupResult name description classes categories 
        
getClassesFromLookup :: String -> Classes
getClassesFromLookup lookupData = 
    dropXmlTagsAndZipUnique 
    . take 1 
    . dropWhile (\a -> not $ "<Classes>" `isPrefixOf` a)
    . fmap trim 
    $ lines lookupData

getDescriptionFromLookup :: String -> Description
getDescriptionFromLookup lookupData = 
    dropTags 
    . take 1 
    . dropWhile (\a -> not $ "<Description>" `isPrefixOf` a) 
    . fmap trim 
    $ lines lookupData
    where
        dropTags []    = [] 
        dropTags [str] = if length res /= 0 then head res else [] 
            where
                res = filter (not . null) 
                    . splitOn "</Description>" 
                    . concat $ filter (not . null) 
                    . splitOn "<Description>" 
                    $ trim str

getCategoriesForLookup :: String -> Categories
getCategoriesForLookup lookupData = 
    dropXmlTagsAndZipUnique 
    . take 1 
    . dropWhile (\a -> not $ "<Categories>" `isPrefixOf` a) 
    . fmap trim 
    $ lines lookupData


lookupElems :: [String] -> IO [LookupResult]
lookupElems [] = return []
lookupElems (a:as) = (:) <$> lookupForName a <*> lookupElems as

getFrequencyList :: [String] -> [(String, Int)]
getFrequencyList list = 
    sortBy (\(_,a) (_,b)->compare b a) 
    . fmap (\a-> 
        let len = length a 
        in if len/=0 
            then (head a, len) 
            else ("", len)) 
    . group 
    $ sort list

prepareText :: String -> String
prepareText = fmap toLower . filter (not . isPunctuation)

processDescriptions :: [LookupResult] -> [String]
processDescriptions = foldl (\acc (LookupResult _ desc _ _) -> ((nub $ words $ prepareText desc) ++ acc)) [] 

processClasses :: [LookupResult] -> [String]
processClasses = foldl (\acc (LookupResult _ _ classes _) -> ((fmap fst classes) ++ acc)) [] 

processCategories :: [LookupResult] -> [String]
processCategories = foldl (\acc (LookupResult _ _ _ categories) -> ((fmap fst categories) ++ acc)) [] 


removeStopWords :: [String] -> [String] -> [String]
removeStopWords stopwords = foldl (\acc a -> if a `notElem` stopwords then (a:acc) else acc) []

test = do
    stopWords <- fmap words $ readFile "data/stopwords.txt"
    testVals <- fmap words $ readFile "data/testVals.txt"
    x <- lookupElems testVals
    -- x <- lookupElems ["cat", "dog", "fish", "duck", "mouse", "chick", "pig", "cow"]
    putStrLn $ (take 40 $ repeat '=') ++ " DESCRIPTION: "  ++ (take 40 $ repeat '=')
    putStrLn . show $ take 10 $ getFrequencyList $ removeStopWords stopWords $ processDescriptions x
    putStrLn ""
    putStrLn $ (take 40 $ repeat '=') ++ "   CLASSES:   "  ++ (take 40 $ repeat '=')
    putStrLn . show $ take 10 $ getFrequencyList $ processClasses x
    putStrLn ""
    putStrLn $ (take 40 $ repeat '=') ++ "  CATEGORIES: " ++ (take 40 $ repeat '=')
    putStrLn . show $ take 10 $ getFrequencyList $ processCategories x

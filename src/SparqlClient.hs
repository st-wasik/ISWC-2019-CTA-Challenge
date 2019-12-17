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

dropXmlTagsAndZip :: [String] -> [(String, String)]
dropXmlTagsAndZip [] = []
dropXmlTagsAndZip (a:_) = foldl (\acc a -> case a of; [a,b]-> ((a,b):acc); _->acc) [] $ chunksOf 2 $ filter (`notElem` xmlTags) $ filter (not . isPrefixOf "/") $ filter (not . null) $ concat $ fmap (splitOn ">") $ splitOn "<" a

lookupForName :: String -> IO LookupResult
lookupForName name = do
    a <- (HTTP.simpleHTTP . HTTP.getRequest $ baseLookupEndpoint ++ name) >>= HTTP.getResponseBody 
    let 
        desc    = getDescriptionFromLookup a
        classes = getClassesFromLookup a
        categories = getCategoriesForLookup a
    return $ LookupResult name desc classes categories 
        
getClassesFromLookup :: String -> Classes
getClassesFromLookup lookupData = dropXmlTagsAndZip $ take 1 $ dropWhile (\a -> not $ "<Classes>" `isPrefixOf` a) $ fmap trim $ lines lookupData

getDescriptionFromLookup :: String -> Description
getDescriptionFromLookup lookupData = dropTags $ take 1 $ dropWhile (\a -> not $ "<Description>" `isPrefixOf` a) $ fmap trim $ lines lookupData
    where
        dropTags []    = [] 
        dropTags [str] = head $ filter (not . null) $ splitOn "</Description>" $ concat $ filter (not . null) $ splitOn "<Description>" $ trim str

getCategoriesForLookup :: String -> Categories
getCategoriesForLookup lookupData = dropXmlTagsAndZip $ take 1 $ dropWhile (\a -> not $ "<Categories>" `isPrefixOf` a) $ fmap trim $ lines lookupData


lookupElems :: [String] -> IO [LookupResult]
lookupElems [] = return []
lookupElems (a:as) = (:) <$> lookupForName a <*> lookupElems as

getFrequencyList :: [String] -> [(String, Int)]
getFrequencyList list = sortBy (\(_,a) (_,b)->compare b a) $ fmap (\a-> (head a, length a)) $ group $ sort list
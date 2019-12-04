module Main where

import CSV 

import Data.List
import Data.Char

import Database.SPARQL.Protocol.Client as S
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Client as HTTP


main :: IO ()
main = do
    targetFileContent <- readFile "data/CTA_Round1_Targets.csv"
    let fileNames = sort . filter (not . null) $ fstColumnValues targetFileContent 
    dataFiles <- mapM readDataFile fileNames
    putStrLn . concat . intersperse "\n" $ fmap (unwords) . fmap (fstLineValues) $ dataFiles


packStr = B.pack . fmap (fromIntegral . ord)
query = "select distinct ?Concept where {[] a ?Concept} LIMIT 100"
endpoint = "http://dbpedia.org/sparql"
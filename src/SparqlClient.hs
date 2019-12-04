module SparqlClient where

import qualified Database.SPARQL.Protocol.Client    as SPARQL
import qualified Data.ByteString.Lazy.Char8         as BSC8
import Text.Pretty.Simple -- pPrint

dbpediaEndpoint = "http://dbpedia.org/sparql"

selectQuery = "select distinct ?Concept where {[] a ?Concept} LIMIT 100"

sendExampleSelect = sendQuery dbpediaEndpoint selectQuery

sendQuery endpoint query = SPARQL.select endpoint (BSC8.pack query) 
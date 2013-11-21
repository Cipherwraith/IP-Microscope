-- Ip microscope
-- 

module Main where

import qualified Data.Map.Lazy as M
import Control.Arrow

data Parsed = Parsed {
    _visitorIp :: String,
    _visitedPage :: String
  } deriving (Show, Eq, Ord)

parse :: String -> Parsed
parse x = Parsed (parseIP x) (parsePage x)
  where
    parseIP = head . words 
    parsePage = (!! 10) . words

countParsed :: [Parsed] -> M.Map Parsed Integer
countParsed = M.fromListWith (+) . flip zip [1,1..]

calculate :: String -> [(Parsed, Integer)]
calculate = M.toList . countParsed . map parse . lines

converge :: [(String, [a])] -> M.Map String [a]
converge = M.fromListWith (++)

formatData :: (Parsed, Integer) -> (String, [(String, Integer)])
formatData = visitorIp &&& pageViews

visitorIp :: (Parsed, Integer) -> String
visitorIp (parsed,_) = _visitorIp parsed

pageViews :: (Parsed, Integer) -> [(String, Integer)]
pageViews (parsed, num) = [(_visitedPage parsed, num)]

prettyPrint (ip, pages) = putStrLn $ 
  concat [ ip, " has visited these pages:\n"
         , concatMap prettyPage pages
         ]
  where
    prettyPage (page, views) = concat [ "    ["
                     , show views
                     , pluralView views
                     , page
                     , "\n"
                     ]
    pluralView 1 = " View] "
    pluralView _ = " Views] "

main = do
  contents <- getContents
  let calculations = M.toList . converge . map formatData . calculate $ contents
  mapM_ prettyPrint calculations

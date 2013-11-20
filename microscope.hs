-- Ip microscope

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
visitorIp = _visitorIp . fst

pageViews :: (Parsed, Integer) -> [(String, Integer)]
pageViews x = [(_visitedPage . fst &&& snd) x]

prettyPut :: String -> IO ()
prettyPut x 
  | null x = return ()
  | otherwise = putStrLn x

prettyPrint q 
  | length (snd q) == 1 = []
  | otherwise = concat [ ip, " has visited these pages:\n"
                       , concatMap pages (snd q)
                       ]
  where
    pages x = concat [ "    ["
                     , show . snd $ x
                     , pluralView $ snd x
                     , fst x
                     , "\n"
                     ]
    ip = fst q
    pluralView x = if x == 1
                    then " View] "
                    else " Views] "

main = do
  contents <- getContents
  let calculations = M.toList . converge . map formatData . calculate $ contents
  mapM_ (prettyPut . prettyPrint) calculations

module Main where
import Data.List
import qualified Data.ByteString.Lazy as B hiding (pack, putStrLn, concatMap)
import qualified Data.ByteString.Lazy.Char8 as B hiding (head, cons, snoc)
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Applicative
import Data.Monoid

type PData = M.Map B.ByteString (M.Map B.ByteString Integer)

parse :: PData -> B.ByteString -> PData 
parse m x = M.insertWith f parseIP parseMap m
  where
    (parseIP:_) = parseWords
    parsePage = parseWords !! 10
    parseWords = B.words x

    parseMap   
      | isNothing lMap = M.fromList [(parsePage, 1)] 
      | otherwise = fromJust lMap

    lMap = M.lookup parseIP m
    f _ =  M.insertWith (+) parsePage 1

folded :: [B.ByteString] -> PData
folded = foldl' parse blank

blank :: PData
blank = M.fromList [(B.empty, M.fromList [(B.empty, 0)])]

pretty :: PData -> [B.ByteString]
pretty = map pretty' . M.toList
  where
    pretty' (y, ys) = mconcat [ y, B.pack " has visited these pages:\n" 
                              , mconcat $ prettyPage (M.toList ys)]
    prettyPage = map printer
    printer (u, v) = mconcat [ B.pack "    [", views
                             , pluralView v
                             , u
                             , B.pack "\n" ]
      where
        views = B.pack . show $ v

pluralView :: Integer -> B.ByteString
pluralView 1 = B.pack " View] "
pluralView _ = B.pack " Views] "

main :: IO ()
main = mapM_ B.putStrLn =<< pretty . folded . B.lines <$> B.getContents


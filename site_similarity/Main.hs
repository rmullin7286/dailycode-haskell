module Main where 

import Data.Set(Set)
import qualified Data.Set as S
import Data.List(sortBy, groupBy, tails)
import Data.Function(on)
import Control.Arrow((&&&), second)

-- You are given a list of (website, user) pairs that represent users visiting websites. Come up with a program that identifies the top k pairs of websites with the greatest similarity.

sortByFirst :: Ord a => [(a, b)] -> [(a, b)]
sortByFirst = sortBy (compare `on` fst)

groupByFirst :: Ord a => [(a, b)] -> [[(a, b)]]
groupByFirst = groupBy ((==) `on` fst)

collapsePairs :: Ord a => [[(a, b)]] -> [(a, [b])]
collapsePairs = map (fst . head &&& map snd)

mapSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSecond = map . second

allPairs :: [a] -> [(a, a)]
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

setCompare :: (String, (Set Int)) -> (String, (Set Int)) -> (String, String, Float)
setCompare (name1, set1) (name2, set2) = (name1, name2, similarity)
    where totalSize = S.size set1 + S.size set2
          intersectionSize = S.size $ S.intersection set1 set2
          similarity = (fromIntegral intersectionSize / fromIntegral totalSize) * 100.0

third :: (a, b, c) -> c
third (_, _, c) = c

mostSimilarSites :: [(String, Int)] -> [(String, String)]
mostSimilarSites = map (\(a, b, _) -> (a, b))
                 . sortBy ((flip compare) `on` third)
                 . map (uncurry setCompare)
                 . allPairs 
                 . mapSecond S.fromList
                 . collapsePairs 
                 . groupByFirst 
                 . sortByFirst

kMostSimilarSites :: Int -> [(String, Int)] -> [(String, String)]
kMostSimilarSites k xs = take k $ mostSimilarSites xs

main = do
    let input = [("a", 1), ("a", 3), ("a", 5), ("b", 2), ("b", 6), ("c", 1), ("c", 2), ("c", 3), ("c", 4), ("c", 5), ("d", 4), ("d", 5), ("d", 6), ("d", 7), ("e", 1), ("e", 3), ("e", 5), ("e", 6)]
    putStrLn $ "Input: " ++ show input
    putStrLn $ "K: 1" 
    putStrLn $ "Expected Output: [('a', 'e')]"
    putStrLn $ "Actual Output: " ++ show (kMostSimilarSites 1 input)
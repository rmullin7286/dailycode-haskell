module Main where

-- INSTRUCTIONS
-- In academia, the h-index is a metric used to calculate the impact of a researcher's papers. It is calculated as follows:
-- A researcher has index h if at least h of her N papers have h citations each. If there are multiple h satisfying this formula, the maximum is chosen.
-- For example, suppose N = 5, and the respective citations of each paper are [4, 3, 0, 1, 5]. Then the h-index would be 3, since the researcher has 3 papers with at least 3 citations.
-- Given a list of paper citations of a researcher, calculate their h-index.

-- The solution to this problem is simple. All we need to do is sort in ascending order, iterate over each element in the list until we find the element where
-- the number of remaining elements in the list plus the current is less than or equal to the number of citations in the current element

import Data.List(sort)

hindex :: [Int] -> Int
hindex xs = hindex' 0 $ sort xs
    where len = length xs
          hindex' _ [] = 0
          hindex' idx (x:xs) = if (len - idx) <= x then (len - idx) else hindex' (idx + 1) xs

main :: IO ()
main = do
    putStrLn "Input: [4, 3, 0, 1, 5]"
    putStrLn "Expected: 3"
    putStrLn $ "Actual: " <> (show $ hindex [4, 3, 0, 1, 5])
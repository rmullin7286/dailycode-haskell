module Main where

import Data.Vector(Vector)
import qualified Data.Vector as V

-- Given a sorted array arr of distinct integers, return the lowest index i 
-- for which arr[i] == i. Return null if there is no such index.

-- For example, given the array [-5, -3, 2, 3], return 2 since arr[2] == 2. 
-- Even though arr[3] == 3, we return 2 since it's the lowest index.

-- SOLUTION:
-- Although an O(N) solution is trivial, we can optimize this further by taking
-- advantage of the fact that the array is sorted. Because of this,
-- we can do a binary search on the array instead of a linear search.

binarySearch' :: Vector Int -> Int -> Int -> Maybe Int -> Maybe Int
binarySearch' arr lo hi prev =
  if lo > hi
  then prev
  else let mid = lo + (hi - lo) `div` 2
       in case compare (arr V.! mid) mid of
            EQ -> binarySearch' arr lo (mid - 1) (Just mid)
            LT -> binarySearch' arr (mid + 1) hi prev
            GT -> binarySearch' arr lo (mid - 1) prev

binarySearch :: Vector Int -> Maybe Int
binarySearch arr = binarySearch' arr 0 (V.length arr - 1) Nothing

main = do
    putStrLn "Input: [-5, -3, 2, 3]"
    putStrLn "Expected: Just 2"
    putStrLn $ "Actual: " ++ show (binarySearch $ V.fromList [-5, -3, 2, 3])
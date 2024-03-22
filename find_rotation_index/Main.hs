-- PROBLEM:
-- A sorted array of integers was rotated an unknown number of times.
-- Given such an array, find the index of the element in the array in faster than linear time. If the element doesn't exist in the array, return null.
-- For example, given the array [13, 18, 25, 2, 8, 10] and the element 8, return 4 (the index of 8 in the array).
-- You can assume all the integers in the array are unique.

import qualified Data.Vector as V

findIndex :: V.Vector Int -> Int -> Maybe Int
findIndex v x = findIndex' 0 $ (V.length v - 1)
  where findIndex' lo hi 
          | lo > hi = Nothing
          | v V.! mid == x = Just mid
          | lo <= mid = if x >= lo && x < mid then findIndex' lo (mid -1) else findIndex' (mid + 1) hi
          | otherwise = if x > mid && x <= hi then findIndex' (mid + 1) hi else findIndex' lo (mid - 1)
          where mid = (lo + hi) `div` 2

main :: IO ()
main = do
  let vec = V.fromList [13, 18, 25, 2, 8, 10]
  let target = 8
  let expected = Just 4
  let actual = findIndex vec target 
  putStrLn $ "Input list: " ++ (show vec)
  putStrLn $ "target: " ++ (show target)
  putStrLn $ "Expected: " ++ (show expected)
  putStrLn $ "Actual: " ++ (show actual)

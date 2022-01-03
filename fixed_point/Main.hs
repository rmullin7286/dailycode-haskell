module Main where

import Data.List

-- A fixed point in an array is an element whose value is equal to its index. Given a sorted array of distinct elements, return a fixed point, if one exists. Otherwise, return False.
-- For example, given [-6, 0, 2, 40], you should return 2. Given [1, 5, 7, 8], you should return False.

-- For this question, we'll be returning Nothing instead of False if there is no fixed point.

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

fixedPoint :: [Int] -> Maybe Int
fixedPoint = (snd <$>) . find (uncurry (==)) . enumerate

main :: IO ()
main = do
    putStrLn "Input: [-6, 0, 2, 40]"
    putStrLn "Expected: Just 2"
    putStrLn $ "Actual: " ++ show (fixedPoint [-6, 0, 2, 40])
    putStrLn "Input: [1, 5, 7, 8]"
    putStrLn "Expected: Nothing"
    putStrLn $ "Actual: " ++ show (fixedPoint [1, 5, 7, 8])
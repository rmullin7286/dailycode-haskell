module Main where

-- Problem:
-- The sequence [0, 1, ..., N] has been jumbled, and the only clue you have for its order
-- is an array representing whether each number is larger or smaller than the last. Given
-- this information, reconstruct an array that is consistent with it. For example, given
-- [None, +, +, -, +], you could return [1, 2, 3, 0, 4].

-- First we count the number of negative values in the relative array to see what our baseline starting value should be. The values can't go below zero, so we need
-- (number of negative values) below the starting value in order to fill out the -'s in the sequence. Then from the starting value, we increment each + by 1 from the starting
-- value, and decrement each - by 1 from the starting value so there are no repeated values in the sequence. This solution requires exactly 2 passes through the input,
-- so the time complexity is O(n), and the space complexity is O(1).

data RelativeValue = None | Positive | Negative deriving (Eq, Show)

startingValue :: [RelativeValue] -> Int
startingValue = length . filter (== Negative)

reconstruct' :: Int -> Int -> [RelativeValue] -> [Int]
reconstruct' _ _ [] = []
reconstruct' hi lo (None:xs) = hi : reconstruct' hi lo xs
reconstruct' hi lo (Positive:xs) = (hi + 1) : reconstruct' (hi + 1) lo xs
reconstruct' hi lo (Negative:xs) = (lo - 1) : reconstruct' hi (lo - 1) xs

reconstruct :: [RelativeValue] -> [Int]
reconstruct xs = let start = startingValue xs in reconstruct' start start xs

main :: IO ()
main = do
    let input = [None, Positive, Positive, Negative, Positive]
    putStrLn "Input: [None,+,+,-,+]"
    putStrLn "Expected: [1,2,3,0,4]"
    putStrLn $ "Actual: " ++ show (reconstruct input)
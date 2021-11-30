module Main where

-- INSTRUCTIONS
-- Given an array of numbers N and an integer k, your task is to split N into k partitions such that the maximum sum of any partition is minimized. Return this sum.

-- To start lets think of the problem backwards. Say we set the maximum sum at X. We have to prove that we can partition the array K times such that the maximum sum of any partition is X.
-- We can do this by grabbing values from the array until the next value would cause the sum to exceed the maximum. If we can make K paritions this way, we know that X is possible.
-- We can use a binary search over the values (0, Sum of array), starting at (Sum of array) / 2, making the check logarithmic rather than linear. So the final complexity would be 
-- O(nlog(sum))

-- drops values from the list until we exceed the sum limit
dropUntilSum :: Int -> [Int] -> [Int]
dropUntilSum sum [] = []
dropUntilSum sum list@(x:xs) = if (sum - x) < 0 then list else dropUntilSum (sum - x) xs

-- This function checks if the maximum sum is possible with K partitions over the list
check k sum [] = k >= 0
check k sum list@(x:_) = if x > sum then False else check (k - 1) sum $ dropUntilSum sum list

-- This is the main function of the problem. We take start, which is the maximum single value in the list (the minimum sum possible given k = length) and end, which is the sum of the list (k = 1)
-- Then, we recursively shift start and end depending on whether or not the middle value (average of the two) is a possible answer. We keep doing this until we reach the minimum possible sum.
partitionMaximum :: Int -> [Int] -> Int
partitionMaximum k xs = partitionMaximum' (maximum xs) (sum xs) 0
    where partitionMaximum' start end answer
            | start > end = answer
            | check k mid xs = partitionMaximum' start (mid - 1) mid
            | otherwise = partitionMaximum' (mid + 1) end answer
                where mid = (start + end) `div` 2 


main = do
    putStrLn "N = [5, 1, 2, 7, 3, 4]"
    putStrLn "K = 3"
    putStrLn "Expected output: 8"
    putStrLn $ "Actual Output: " <> (show $ partitionMaximum 3 [5,1,2,7,3,4])

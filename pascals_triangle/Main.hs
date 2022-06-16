module Main where

-- The goal of this problem is to determine the number in the nth row of pascals triangle.
-- Pascal's triangle is a triangle of numbers where each element in a row is the sum of the two elements above it.
-- For example:
--      1
--     1 1
--    1 2 1
--   1 3 3 1
--  1 4 6 4 1
-- 1 5 10 10 5 1

recursiveSolution :: Int -> [Int]
recursiveSolution 0 = [1]
recursiveSolution n = zipWith (+) (0 : previous) (previous ++ [0])
  where previous = recursiveSolution (n-1)

main :: IO ()
main = do
  putStrLn "Input: 5. Expected: [1,5,10,10,5]"
  putStrLn $ "Recursive: " ++ show (recursiveSolution 5)


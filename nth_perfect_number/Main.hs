module Main where

-- So I know that technically you can determine this number quicker by iterating 9i + 19 instead of through every number,
-- but I don't know how to prove this deductively or inductively, so I'm just going to do it the slow way

digitSum :: Int -> Int
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

isPerfect :: Int -> Bool
isPerfect = (== 10) . digitSum

-- This infinite list will memoize the results so we don't have to recalculate them
perfects :: [Int]
perfects = filter isPerfect [1..]

perfectN :: Int -> Int
perfectN = (perfects !!) . (subtract 1)

main = do
  putStrLn "Input 1. Expected 19"
  print $ perfectN 1
  putStrLn "Input 2. Expected 28"
  print $ perfectN 2
  putStrLn "Input 5: Expected 55"
  print $ perfectN 5


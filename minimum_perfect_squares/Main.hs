module Main where

perfectSquares :: [Int]
perfectSquares = [x^2 | x <- [1..]]

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

isPerfectSquare :: Int -> Bool
isPerfectSquare n = isInt $ sqrt $ fromIntegral n

getHighestSquare :: Int -> Int
getHighestSquare n = getHighestSquare' n perfectSquares
    where getHighestSquare' n (x:y:xs) = if y > n then x else getHighestSquare' n (y:xs)

minimumPerfectSquares :: Int -> Int
minimumPerfectSquares 0 = 0
minimumPerfectSquares n = if isPerfectSquare n then 1 else 1 + minimumPerfectSquares (n - getHighestSquare n)

main :: IO ()
main = do 
    print $ minimumPerfectSquares 63
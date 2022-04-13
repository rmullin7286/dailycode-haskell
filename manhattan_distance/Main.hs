module Main where

import Data.List(minimumBy)

-- You are writing an AI for a 2D map game. You are somewhere in a 2D grid, 
-- and there are coins strewn about over the map.

-- Given the position of all the coins and your current position, 
-- find the closest coin to you in terms of Manhattan distance. 
-- That is, you can move around up, down, left, and right, but not diagonally.
-- If there are multiple possible closest coins, return any of them.

-- example:
-- Our position: (0, 2)
-- Coins: [(0, 4), (1, 0), (2, 0), (3, 2)]
-- Answer: (0, 4)

-- SOLUTION
-- The solution to this problem is really easy. Given that there are never any blocking walls between two points, 
-- the minimum distance will always be abs(x1 - x2) + abs(y1 - y2).
-- So we just check that for every coin, and find the shortest distance.

data Point = Point Int Int deriving (Show, Eq)

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

shortestDistance :: [Point] -> Point -> Point
shortestDistance coins current = minimumBy (\a b -> compare (manhattan a current) (manhattan b current)) coins

main :: IO ()
main = do
    putStrLn "Our position: (0, 2)"
    putStrLn "Coins: [(0, 4), (1, 0), (2, 0), (3, 2)]"
    putStrLn "Expected: Point 0 4"
    putStrLn $ "Actual: " ++ (show $ shortestDistance [(Point 0 4), (Point 1 0), (Point 2 0), (Point 3 2)] (Point 0 2))
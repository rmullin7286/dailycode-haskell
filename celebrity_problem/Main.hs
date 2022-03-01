module Main where

import Data.Vector(Vector)
import qualified Data.Vector as V

-- At a party, there is a single person who everyone knows, but who does not know anyone in return (the "celebrity"). 
-- To help figure out who this is, you have access to an O(1) method called knows(a, b), which returns True if person a knows person b, else False.

-- Given a list of N people and the above operation, find a way to identify the celebrity in O(N) time.

-- SOLUTION:
-- This problem can be solved recursively.

-- Let's think of the base case: there are 0 people at the party. In this case, there is no celebrity.
-- Recursively, given the potential celebrity of the attendees from 0..N-1:
-- If there is no potential celebrity in the set from 0..N-1, then attendee N is the potential celebrity.
-- If there is a potential celebrity in the set and they are known by N, then they cannot be the celebrity, so N is the potential celebrity.
-- If there is a potential celebrity in the set and N is known by them, then N cannot be the celebrity, so they will continue to be the potential celebrity.
-- If neither the potential or N know each other, neither can be the celebrity, so there is no celebrity from 0..N.

-- Once we've found the potential, all we have to do is ensure that everyone knows them, and they know everyone else.
-- In total, the recursive step will be called N times, and we need N checks to ensure who knows who, so the runtime will be O(N).


-- Since the test doesn't give any inputs we'll just hardcode the knowledge map. Person 3 will be the celebrity.
-- Note that the celebrity knows themselves so we have to check for that.
partySize = 5

from2dList :: [[a]] -> Vector (Vector a)
from2dList = V.fromList . map V.fromList

kmap = from2dList [ [True, False, True, False, False]
                  , [False, True, True, True, False]
                  , [False, False, True, False, False]
                  , [False, False, True, True, False]
                  , [False, False, True, False, True]
                  ]

knows :: Int -> Int -> Bool
knows a b = kmap V.! a V.! b

findPotentialCelebrity :: Int -> Maybe Int
findPotentialCelebrity 0 = Nothing
findPotentialCelebrity n = 
    let potential = findPotentialCelebrity (n-1)
    in case potential of
        Nothing -> Just (n - 1)
        Just p -> if p `knows` (n - 1) then Just (n - 1)
                  else if (n - 1) `knows` p then Just p
                  else Nothing

checkIfCelebrity :: Int -> Maybe Int
checkIfCelebrity n = if all (\p -> p == n || (p `knows` n && not (n `knows` p))) [0..partySize - 1] then Just n else Nothing

-- This is where we find the celebrity
celebrity :: Maybe Int
celebrity = (+ 1) <$> (findPotentialCelebrity partySize >>= checkIfCelebrity)

main :: IO ()
main = do
    putStrLn "Party member 3 is the celebrity"
    putStrLn $ "Actual Answer: " ++ (show celebrity)

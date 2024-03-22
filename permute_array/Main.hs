-- PROBLEM:
-- A permutation can be specified by an array P, where P[i] represents the location of the
-- element at i in the permutation. For example, [2, 1, 0] represents the permutation where 
-- elements at the index 0 and 2 are swapped.

-- Given an array and a permutation, apply the permutation to the array.
-- For example, given the array ["a", "b", "c"] and the permutation [2, 1, 0], return ["c", "b", "a"].

-- SOLUTION:
-- This is a pretty straightforward problem. Just build a new array by indexing the old one.
import qualified Data.Vector as V

permute :: V.Vector a -> V.Vector Int -> V.Vector a
permute v ps = V.generate (V.length ps) (\i -> v V.! (ps V.! i)) 

main :: IO ()
main = do
  let array = V.fromList ['a', 'b', 'c']
  let permutations = V.fromList [2, 1, 0]
  let expected = V.fromList ['c', 'b', 'a']
  let actual = permute array permutations
  putStrLn $ "array: " ++ (show array)
  putStrLn $ "permutations: " ++ (show array)
  putStrLn $ "expected: " ++ (show expected)
  putStrLn $ "actual: " ++ (show actual)

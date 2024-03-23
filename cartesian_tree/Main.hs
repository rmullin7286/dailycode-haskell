-- PROBLEM:
-- A Cartesian tree with sequence S is a binary tree defined by the following two properties:
-- It is heap-ordered, so that each parent value is strictly less than that of its children.
-- An in-order traversal of the tree produces nodes with values that correspond exactly to S.
-- For example, given the sequence [3, 2, 6, 1, 9], the resulting Cartesian tree would be:
--
--                   1
--                2    9
--              3   6
--
-- SOLUTION:
-- To solve this, we need to build up the tree from the bottom left node in an order such that if we
-- traverse the tree from as if it was ordered, it would print the same result. To do so, we can follow
-- these steps:
-- 1. Begin with a single root node equal to the value of the head of the list
-- 2. For each next element in the list, do the following:
--  2a. Traverse up the tree until we find a node that is less than the head of the list
--  2b. If we find a node that satisfies the condition, insert the head as the right child, and make the
--      previous right child the left child of the new node. Repeat starting at the new node.
--  2c. If we don't find a node that satisfies the condition, make the root node the left child of a new root equal
--      to the head of the list. Repeat step 2 with the new root.

data Tree a 
  = Leaf 
  | Branch a (Tree a) (Tree a)
  deriving (Show)

cartesianTree = fst . cartesianTree' True Leaf

cartesianTree' _ t [] = (t, [])
cartesianTree' _ Leaf (x:xs) = cartesianTree' True (Branch x Leaf Leaf) xs
cartesianTree' isRoot node@(Branch n l r) (x:xs)
  | n < x = cartesianTree' isRoot (Branch n l newr) newxs
  | isRoot = cartesianTree' True (Branch x node Leaf) xs
  | otherwise = (node, (x:xs))
  where (newr, newxs) = cartesianTree' False (Branch x r Leaf) xs

main = do
  let input = [3, 2, 6, 1, 9]
  let expected = (Branch 1
                  (Branch 2
                    (Branch 3 Leaf Leaf)
                    (Branch 6 Leaf Leaf))
                  (Branch 9 Leaf Leaf))
  let output = cartesianTree input :: Tree Int
  putStrLn $ "input: " ++ show (input)
  putStrLn $ "Expected: " ++ show (expected)
  putStrLn $ "Actual: " ++ show (output)

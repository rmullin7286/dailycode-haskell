module Main where

-- this program merges two binary trees. Two nodes in the same position in both trees will equal the sum of both.
-- If only one tree has a node at a position, it will equal that value.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

merge :: (Num a) => Tree a -> Tree a -> Tree a
merge Empty Empty = Empty
merge Empty notEmpty@(Node _ _ _) = notEmpty
merge notEmpty@(Node _ _ _) Empty = notEmpty
merge (Node x l1 r1) (Node y l2 r2) = Node (x + y) (merge l1 l2) (merge r1 r2)

main :: IO ()
main = do
  let tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
  let tree2 = Node 4 (Node 5 (Node 7 Empty Empty) Empty) (Node 6 Empty Empty)
  let expected = Node 5 (Node 7 (Node 7 Empty Empty) Empty) (Node 9 Empty Empty)
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Actual: " ++ show (merge tree1 tree2)
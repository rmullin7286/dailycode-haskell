module Main where

import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)

data Tree a = Branch (Tree a) a (Tree a) | Leaf

flatten :: Tree a -> [a]
flatten (Branch l x r) = mconcat [[x], flatten l, flatten r]
flatten Leaf = []

recursiveSums Leaf = Leaf
recursiveSums (Branch l x r) = Branch newL (value newL + x + value newR) newR
  where
    newL = recursiveSums l
    newR = recursiveSums r

value (Branch _ x _) = x
value Leaf = 0

answer =
  listToMaybe
    . map fst
    . sortBy (flip compare `on` snd)
    . M.toList
    . foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty
    . flatten
    . recursiveSums

main = do
  let input =
        Branch
          (Branch Leaf 2 Leaf)
          5
          (Branch Leaf (-5) Leaf)

  let output = answer input

  putStrLn "Expected: 2"
  putStrLn $ "Actual: " <> show output

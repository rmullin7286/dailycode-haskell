module Main where

import Data.List(isPrefixOf)
import Data.Monoid

-- INSTRUCTIONS
-- Implement a PrefixMapSum type with two operations
-- insert key value: Insert the key value pair into the map
-- sum prefix: sum all values in the map with the given key prefix

-- The instructions are unclear whether or not we're able to use a predefined Map implementation, so we'll create a really simple one based off of a Binary Search Tree
data PrefixMapSum v = Node (PrefixMapSum v) String v (PrefixMapSum v) | Empty

emptyPrefixMapSum :: PrefixMapSum v
emptyPrefixMapSum = Empty

fromList :: [(String, v)] -> PrefixMapSum v
fromList = foldr (\(k, v) m -> insert k v m) Empty

insert :: String -> v -> PrefixMapSum v -> PrefixMapSum v
insert key value Empty = Node Empty key value Empty
insert key value (Node l k v r) 
    | k == key = Node l key value r
    | k > key = Node (insert key value l) k v r
    | otherwise = Node l k v (insert key value r)

sumMap :: Num v => String -> PrefixMapSum v -> v
sumMap prefix pms = getSum $ sum' prefix pms
    where sum' prefix (Node l k v r) = (if prefix `isPrefixOf` k then Sum v else mempty) <> sum' prefix l <> sum' prefix r
          sum' _ Empty = mempty

main :: IO ()
main = do
    let map = emptyPrefixMapSum

    let map' = insert "columnar" 3 map
    putStrLn $ "summing on \"col\". Expected: 3. Actual: " <> (show $ sumMap "col" map')

    let map'' = insert "column" 2 map'
    putStrLn $ "summing on \"col\". Expected: 5. Actual: " <> (show $ sumMap "col" map'')
module Main where

import qualified Data.Map as M
import Data.Map(Map)

-- Ghost is a two-person word game where players alternate appending letters to a word. The first person who spells out a word, or creates a prefix for which there is no possible continuation, loses. Here is a sample game:

-- Player 1: g
-- Player 2: h
-- Player 1: o
-- Player 2: s
-- Player 1: t [loses]
-- Given a dictionary of words, determine the letters the first player should start with, such that with optimal play they cannot lose.

-- NOTE: The official instructions for this question are incorrect. They assert given the dictionary ["cat", "calf", "bear", "dog"], only b is a winning letter. Actually c is also a winning letter, since the path (P1 'c') -> (P2 'a') -> (P1 'l') -> (P2 'f') can be forced.

newtype Trie = Trie (Map Char Trie)

data Player = P1 | P2

unTrie :: Trie -> Map Char Trie
unTrie (Trie m) = m

isNull :: Trie -> Bool
isNull = M.null . unTrie

childTries :: Trie -> [Trie]
childTries = M.elems . unTrie

empty :: Trie
empty = Trie M.empty

insertWord :: String -> Trie -> Trie
insertWord [] t = t
insertWord (x:xs) (Trie m) = Trie $ M.alter updateEntry x m
    where updateEntry Nothing = Just (insertWord xs (Trie M.empty))
          updateEntry (Just t) = Just (insertWord xs t)

buildDictionaryTrie :: [String] -> Trie
buildDictionaryTrie = foldl (flip insertWord) empty

foldChildren :: (a -> Char -> Trie -> a) -> a -> Trie -> a
foldChildren f init (Trie m) = M.foldlWithKey f init m

-- base case: If we are at an empty Trie, the player whose turn it was last loses.
-- recursive case on P1 turn: If the there exists any path that results in a guaranteed win, then the player has won
-- recursive case on P2 turn: If all paths result in a guaranteed win for P1, then the player has one.
isWinningPath :: Player -> Trie -> Bool
isWinningPath P1 t =  if isNull t then True else any (isWinningPath P2) (childTries t)
isWinningPath P2 t =  if isNull t then False else all (isWinningPath P1) (childTries t)

ghostGame :: [String] -> [Char]
ghostGame dictionary = foldChildren (\acc c t -> if isWinningPath P2 t then c : acc else acc) [] $ buildDictionaryTrie dictionary

main :: IO ()
main = do
    let dictionary = ["cat", "calf", "dog", "bear"]
    putStrLn $ "Dictionary is " ++ (show dictionary)
    putStrLn "Expected results are \"c\" for \"calf\" and \"b\" for \"bear\""
    putStrLn $ "Actual results are " ++ (show $ ghostGame dictionary)
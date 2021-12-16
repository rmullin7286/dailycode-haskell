module Main where

-- In Ancient Greece, it was common to write text with the first line going left to right, the second line going right to left, and continuing to go back and forth. This style was called "boustrophedon".

-- Given a binary tree, write an algorithm to print the nodes in boustrophedon order.

-- To solve this, we have a recursive function that takes three parameters: The current direction, a stack of nodes on the current level, and a stack of nodes on the next level.
-- First, we insert the top node into the current stack.
-- On each recursive step, we first check to see if both stacks are empty. if they are, return the empty list
-- Otherwise, if the current stack is empty, we swap the next stack for the current stack and reset the next stack to empty
-- If there are nodes in the current stack, pop the top node. Depending on the order, insert the children into the "next" stack from left to right or right to left. Then we recursively call the function with the value of the current node as the head, and the result
-- of the recursive call as the tail.

data Tree a = Empty | Node (Tree a) a (Tree a)

data Direction = L | R
    deriving(Eq)

otherDirection :: Direction -> Direction
otherDirection L = R
otherDirection R = L

newtype Stack a = Stack [a]

instance Semigroup (Stack a) where
    (Stack xs) <> (Stack ys) = Stack (xs ++ ys)

instance Monoid (Stack a) where
    mempty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

singleton :: a -> Stack a
singleton x = Stack [x]

isNull :: Stack a -> Bool
isNull (Stack []) = True
isNull _ = False

getChildrenInOrder :: Direction -> Tree a -> [Tree a]
getChildrenInOrder _ Empty = []
getChildrenInOrder _ (Node Empty x Empty) = []
getChildrenInOrder _ (Node Empty x r) = [r]
getChildrenInOrder _ (Node l x Empty) = [l]
getChildrenInOrder L (Node l x r) = [l, r]
getChildrenInOrder R (Node l x r) = [r, l]

boustrophedon :: Tree a -> [a]
boustrophedon Empty = []
boustrophedon node = boustrophedon' L (singleton node) mempty
    where boustrophedon' dir current next 
            | isNull current && isNull next = []
            | isNull current = boustrophedon' (otherDirection dir) next mempty
            | otherwise = let (x, current') = pop current 
                        in case x of
                            Empty -> boustrophedon' dir current' next
                            node@(Node _ x _) -> x : boustrophedon' dir current' (foldl (flip push) next (getChildrenInOrder dir node))

main = do
    let testTree = Node (Node (Node Empty 4 Empty) 2 (Node Empty 5 Empty) ) 1 (Node (Node Empty 6 Empty) 3 (Node Empty 7 Empty))
    putStrLn "Tree is: "
    putStrLn "   1   "
    putStrLn "  2 3  "
    putStrLn "4 5 6 7"
    putStrLn "Expected output: [1,3,2,4,5,6,7]"
    putStrLn $ "Actual output: " ++ (show $ boustrophedon testTree)
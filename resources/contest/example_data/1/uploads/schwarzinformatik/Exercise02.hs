module Exercise02 where

import Data.List ( sort )

data Tree = Leaf | Node Tree Int Tree
    deriving Show

--
-- for all prices m:
--  for all prices y:
--    if(y >= x) add m
startupRevenue :: [Int] -> Int
startupRevenue xs = maximum prices
    where 
        sorted = sort xs
        prices = [(sorted !! i) * (length xs - i) | i <- [0..(length xs-1)]]

        findMax ss i 
            -- ende der liste
            | i == length xs - 1 = prices !! i
            | prices !! i > prices !! (i + 1) = prices !! i
            | otherwise = findMax ss (i + 1)


-- calcc :: [Int] -> Int -> Int -> Int -> Int
-- calcc xs length length acc = acc
-- calcc xs length index acc = calcc xs length (index+1) newacc
--     where
--         newacc = if rechnen > acc then rechnen else acc
--         rechnen = (xs !! i) * (length - i)

-- buildTree :: Tree -> [Int] -> Tree
-- buildTree = foldl insert

-- createTree :: Int -> Tree
-- createTree x = Node Leaf x Leaf

-- isLeaf :: Tree -> Bool
-- isLeaf Leaf = True
-- isLeaf _ = False

-- insert :: Tree -> Int -> Tree
-- insert Leaf insert = Node Leaf insert Leaf
-- insert (Node Leaf x Leaf) i
--     | i <= x = Node (Node Leaf i Leaf) x Leaf
--     | otherwise = Node Leaf x (Node Leaf i Leaf)

-- insert (Node lT x rT) i
--     | i <= x && isLeaf lT = Node (Node Leaf i Leaf) x rT
--     | i <= x = Node (insert lT i) x rT
--     | isLeaf rT = Node lT x (Node Leaf i Leaf)
--     | otherwise = Node lT x (insert rT i)

-- countGreater :: Tree -> Int -> Int
-- countGreater Leaf _ = 0
-- countGreater (Node lT x rT) v
--     | v <= x = countGreater lT v + countGreater rT v + v
--     | otherwise = countGreater rT v
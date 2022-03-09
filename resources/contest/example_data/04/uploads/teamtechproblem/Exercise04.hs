module Exercise04 where

import Data.List

--data Tree = Node [Tree] | Leaf | L | R
--    deriving Show
--
--instance Eq Tree where
--    Node _ == Node _ = True -- important!
--    Leaf == Leaf = True
--    L == L = True
--    R == R = True
--    _ == _ = False

--firstStage :: String -> String
--firstStage :: [Either Char Tree] -> [Tree] -> String -> [Either Char Tree]
--firstStage rest leaves [] = reverse (Right (Node leaves) : rest)
--firstStage rest leaves ('R':'L':xs) = firstStage rest (Leaf:leaves) xs
--firstStage rest [] (x:xs) = firstStage ((Left x) : rest) [] xs
--firstStage rest leaves (x:xs) = firstStage (Left x : Right (Node leaves) : rest) [] xs

--secondStage :: [Either Char Tree] -> [Either Char Tree]
--secondStage rest collector [] = reverse rest -- wrong?
--secondStage rest [] (Left 'R' : Right node : xs) = secondStage rest [node] xs
--secondStage rest collector (Right node : xs) = secondStage rest (node:collector) xs
--secondStage rest [] (Left 'L' : xs) = secondStage (Left 'L' : rest) [] xs
--secondStage rest collector (Left 'L' : xs) = secondStage ((Node (reverse collector)) : rest) [] xs
--secondStage rest collector (x:xs) = secondStage (x:rest) collector xs

--replaceRLByX :: String -> [Tree]
--replaceRLByX [] = []
--replaceRLByX ('R':'L':xs) = Leaf : replaceRLByX xs
--replaceRLByX ('R':xs) = R : replaceRLByX xs
--replaceRLByX ('L':xs) = L : replaceRLByX xs
--
--groupAndCollect xs = concat $ map someFun $ group xs where
--    someFun xs@(Leaf:_) = [Node xs]
--    someFun xs@(Node _:_) = [Node xs]
--    someFun xs = xs
--
--groupAndCollect2 xs = concat $ boba $ groupBy onlyNodes xs where
--    boba [] = []
--    boba ((R:rs) : m@(Node _:_) : (L:ls) : xs) = rs : [Node m] : ls : boba xs
--    boba (x:xs) = x : boba xs
--
--onlyNodes (Node _) (Node _) = True
--onlyNodes _ _ = False
--allPrefixesAndRest xs = case hasValid xs of {
--        (steps,rest) -> steps : allPrefixes rest;
--        Nothing -> []
--    }


rudolph :: String -> (Int, Int) 
rudolph s = counter s 0 0 (0,0)

--      amount length
--counter pos current_length (amount, length
counter [] 0 len (0,0) = (0,1)
counter [] 0 len (n,m) = (if len /= 0 then (if len > n then len else n, m+1) else (n,m))
counter [] _ _ (n,m) = (n,m)
-- abort the run and restart
counter ('L':xs) 0 len (n,m) = counter xs 0 0 (if len /= 0 then (if len > n then len else n, m+1) else (n,m))
counter ('L':xs) pos len (n,m) = counter xs (pos-1) (len+1) (n,m)
counter ('R':xs) pos len (n,m) = counter xs (pos+1) (len+1) (n,m)

hasValid [] = Nothing
hasValid ('L':xs) = Nothing
hasValid ('R':xs) = hasValidHelper 1 1 xs

hasValidHelper 0 steps xs = Just (steps, xs)
hasValidHelper _ _ [] = Nothing
hasValidHelper n steps ('R':xs) = hasValidHelper (n+1) (steps+1) xs
hasValidHelper n steps ('L':xs) = hasValidHelper (n-1) (steps+1) xs

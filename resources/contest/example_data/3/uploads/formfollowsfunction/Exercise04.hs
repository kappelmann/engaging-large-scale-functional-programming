module Exercise04 where
import Data.List
import Data.Ord

rudolph' :: String -> (Int, Int) 
rudolph' s = (length (maximumBy (comparing length) possible), length possible)
    where
        possible = [[y | y <- possiblePathsWithLength x, y `isSubsequenceOf` s] | x <- [2..length s]]

--dropWhile (!= 'R') s

rudolph :: String -> (Int, Int) 
rudolph s
    | containsValid s = (0, 1)
    | otherwise = let d = startR s in if fst d == 0 then (0, 1) else d

startR :: String -> (Int, Int)
startR xs = aux xs (0, 0)
    where
        aux [] (n, m) = (n, m)
        aux (s:ss) (n, m) 
            | s == 'R' = let d = ruh (s:ss) in aux (drop d ss) $ (if n > d then n else d,m + 1)
            | otherwise  = aux ss (n, m)

ruh :: String -> Int
ruh xs = substrings xs (0, 0) 0
    where 
        substrings [] (x, y) n = 0
        substrings (s:ss) (x, y) n
                                | x > y = 0
                                | s == 'L' && x + 1 == y = let subs = substrings ss (x + 1, y) (n + 1) in 
                                    if subs > (n + 1) then subs else n + 1
                                | s == 'R' = substrings ss (x, y + 1) (n + 1)
                                | s == 'L' = substrings ss (x + 1, y) (n + 1)

possiblePathsWithLength :: Int -> [String]
possiblePathsWithLength n
   | n < 2 || odd n = []
   | otherwise = ["RL" ++ concat x | x <- permutations (replicate (n `div` 2) "LR")]

-- RRRLLL RRLRLL

-- LLRRLRLL
-- RRRLLLLRRLRLLRRRLLL


-- LRRRLLLLRRLRL
-- LRRRLLLLRRLR
-- LRRRLLLLRRL
-- LRRRLLLLRR
-- LRRRLLLLR
-- LRRRLLLL
-- LRRRLLL
-- LRRRLLL
-- []
-- RRRLL
-- ...

-- RLLLLRRLL

-- RRRLLL RRLL RRLRLL 
-- OXXXXXXXXXXXXXXXXXXXXXXXXX
-- OXXXX


containsValid :: String -> Bool
containsValid s = any (\x -> not (x == 'L' || x == 'R'))  s



isAtWarehouse :: String -> Bool
isAtWarehouse s = r == l
    where 
        r = length $ filter (== 'R') s 
        l = length $ filter (== 'L') s


module Exercise04 where

rudolph :: String -> (Int, Int)
rudolph str = if fst r == 0 then (0, 1) else r
    where r = rudolph' str (0, 1)

rudolph' :: String -> (Int, Int) -> (Int, Int)
rudolph' [] (max, cnt) = (max, cnt)
rudolph' str@(s:ss) (max, cnt)
    | maxL > max    = rudolph' ss (maxL, 1)
    | maxL == max   = rudolph' ss (max, cnt + 1)
    | otherwise     = rudolph' ss (max, cnt)
    where maxL = maxLength str 0 0 0

maxLength :: String -> Int -> Int -> Int -> Int
maxLength [] l depth max
    | depth == 0    = l
    | otherwise     = max
maxLength (s:str) l depth max
    | depth < 0     = max
    | depth == 0    = if s == 'L' then l else maxLength str (l+1) (depth + 1) l
    | s == 'L'      = maxLength str (l+1) (depth - 1) max
    | otherwise     = maxLength str (l+1) (depth + 1) max

example = "LRRRLLLLRRLRLL"

{-
rudolph :: String -> (Int, Int) 
rudolph s
    | null l    = (0, 1)
    | otherwise = (max, length $ filter (==max) l)
    where
        l = concat $ lengths' s
        max = maximum l
    

lengths' :: String -> [[Int]]
lengths' [] = []
lengths' str@(s:ss) = tail (lengths str 0 0) : lengths' ss

lengths :: String -> Int -> Int -> [Int]
lengths [] l depth
    | depth == 0 = [l]
    | otherwise = []
lengths (s:str) l depth
    | depth < 0     = []
    | depth == 0    = l : if s == 'L' then [] else lengths str (l+1) (depth + 1)
    | s == 'L'      = lengths str (l+1) (depth - 1)
    | otherwise     = lengths str (l+1) (depth + 1)
-}

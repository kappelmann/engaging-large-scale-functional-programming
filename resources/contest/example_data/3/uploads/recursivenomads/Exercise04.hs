module Exercise04 where

find :: String -> Int -> Int -> Int -> Maybe Int
find ('L':_) 0 0 0 = Nothing
find ('L':_) 0 _ k = Just k
find [] 0 _ k = Just k -- whole word
find [] _ 0 _ = Nothing
find [] _ n _ = Just n -- max length finished
find ('R':ss) 0 _ k = find ss 1 k (k + 1)
--find ('L':ss) 0 _ k = find ss (-1) k (k + 1)
find ('R':ss) a n k = find ss (a + 1) n (k + 1)
find ('L':ss) a n k = find ss (a - 1) n (k + 1)

rudolphAcc :: String -> Int -> Int -> (Int, Int)
rudolphAcc [] n m = (n,m)
rudolphAcc ss n m = case find ss 0 0 0 of
    Just nn ->  if nn > n then rudolphAcc subSS nn 1 
                else if n == nn then rudolphAcc subSS n (m+1)
                else rudolphAcc subSS n m  
                    where   subSS = drop nn ss
    Nothing -> rudolphAcc subSS n m
                    where   subSS = drop 1 ss

rudolph :: String -> (Int, Int)
rudolph ss = rudolphAcc ss 0 1


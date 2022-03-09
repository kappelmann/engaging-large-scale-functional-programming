module Exercise04 where

import Data.List
rudolph :: String -> (Int, Int)
rudolph str = ff str [] 0 (0,1)

isValid :: String -> Int ->Bool
isValid _ (-1) = False
isValid [] val = val == 0
isValid (x:xs) val
    |x == 'L' = isValid xs (val -1)
    |otherwise = isValid xs (val + 1)


getNumbers :: String -> [Int] -> [Int]
getNumbers [] xs = xs
getNumbers (s:str) []
    |s=='R' = getNumbers str [1]
    |otherwise = getNumbers str []
getNumbers (s:str) xs
    |s=='R' = getNumbers str ((head xs +1) : xs)
    |otherwise = getNumbers str ((head xs -1) : xs)

ff :: String -> [Int] -> Int -> (Int, Int) -> (Int, Int)
ff [] _ i acc = verrechne i acc
ff ('R':str) xs i acc = ff str (i:map (+1) xs) 0 (verrechne i acc)
ff ('L':str) (x:xs) i acc = ff str (map (+1) xs) (x + 2) acc
ff ('L':str) [] i acc = ff str [] 0 (verrechne i acc)

verrechne :: Int -> (Int, Int) -> (Int, Int)
verrechne i (a,b)
    |i==0 = (a,b)
    |i < a = (a,b)
    |i == a = (a,b+1)
    |i > a = (i,1)
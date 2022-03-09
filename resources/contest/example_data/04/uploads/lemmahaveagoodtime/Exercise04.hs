module Exercise04 where

import Data.List(subsequences)

rudolph :: String -> (Int, Int) 
rudolph = solution

solution :: String -> (Int, Int) 
solution s = countStrings (filter (\x -> length x == maxL) valids)
    where
        newString = s --cutOutUntilR s
        substrings = createAllSubstrings newString
        valids = filter validSubString substrings
        maxL = maxLength valids

myPrint :: String -> [String] 
myPrint s = filter (\x -> length x == maxL) valids
    where
        newString = s --cutOutUntilR s
        substrings = createAllSubstrings newString
        valids = filter validSubString substrings
        maxL = maxLength valids

countStrings :: [[Char]] -> (Int, Int)
countStrings [] = (0, 1)
countStrings xs = (length (head xs), len) 
    where len = length xs

cutOutUntilR :: [Char] -> [Char]
cutOutUntilR [] = []
cutOutUntilR (x:xs) = if x == 'R' then xs else cutOutUntilR xs

count :: [Char] -> (Int, Int) -> (Int, Int)
count [] f = f
count (x:xs) (l, r) 
    | x == 'L' = count xs (l+1, r)
    | otherwise = count xs (l, r+1)

validSubString :: [Char] -> Bool
validSubString xs = startsWithR xs && (l == r) && rightLeftCount xs (0, 0)
    where (l, r) = count xs (0, 0)

rightLeftCount :: [Char] -> (Int, Int) -> Bool
rightLeftCount [] (l, r) = r >= l
rightLeftCount (x:xs) (l, r)
    | r < l = False  
    | x == 'L' = rightLeftCount xs (l+1, r)
    | otherwise = rightLeftCount xs (l, r+1)

startsWithR :: [Char] -> Bool
startsWithR [] = False 
startsWithR (x:xs) = x == 'R'

createAllSubstrings :: [Char] -> [[Char]]
createAllSubstrings xs = [drop j (take i xs) | i <- [1..len], j <- [i+1..len+1]]
    --[take j (drop i xs) | j <- [0..len], i <- [0..len]]
    where len = length xs

maxLength :: [[Char]] -> Int
maxLength xss = maximum (map length xss)


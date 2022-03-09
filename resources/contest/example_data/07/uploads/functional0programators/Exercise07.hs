module Exercise07 where

import Data.List (sortBy, sort)
import Data.Maybe

main :: IO ()
main = 
    do 
       line <- getLine 
       let num = getNumber line 0
       do
           ps <- readLoop num 1 []
           let res = find (sort ps) [] [] 
           
           maybe (print "-1") printRes res

{-
find :: [(Integer,Integer,Integer)] -> [Integer] -> [(Integer,Integer)] -> Maybe [(Integer,Integer)]
find [] ys res | length ys - 1 > length res = Nothing
               | otherwise = Just res
-}
find :: [(Integer,Integer,Integer)] -> [Integer] -> [(Integer,Integer)] -> Maybe [(Integer,Integer)]
find [] _ res = Just res
               
find ((_,i,j):xs) ys res | i/= j && (notElem i ys || notElem i ys) = find xs (i:j:ys) ((i,j):res) 
                         | otherwise = find xs ys res

printRes :: [(Integer,Integer)] -> IO()
printRes [] = return ()
printRes ((i,j):xs) = do 
                        print (show i ++  " " ++ show  j)
                        printRes xs

readLoop :: Integer -> Integer -> [(Integer,Integer,Integer)] -> IO [(Integer,Integer,Integer)]
readLoop n i xs =   do 
                    line <- getLine
                    let newList = parseNumberLine line i 0 0 xs
                    if i < n then readLoop n (i+1) newList else return newList

parseNumberLine :: String -> Integer -> Integer -> Integer -> [(Integer,Integer,Integer)]  -> [(Integer,Integer,Integer)]
parseNumberLine [] _ _ _ xs = xs
parseNumberLine (s:str) i j c xs | s == ' ' = parseNumberLine str i (j+1) 0 ((c,i,j):xs)
                                 | null str = (c*10 + parseNumber s, i,j):xs
                                 | j <= i  = parseNumberLine str i j (c*10 + parseNumber s) xs
                                 | otherwise = xs


parseNumber :: Char  -> Integer
parseNumber s   | s == '0' = 0
                | s == '1' = 1
                | s == '2' = 2
                | s == '3' = 3
                | s == '4' = 4
                | s == '5' = 5
                | s == '6' = 6
                | s == '7' = 7
                | s == '8' = 8
                | s == '9' = 9

getNumber :: String -> Integer -> Integer
getNumber [] c = c
getNumber (s:ss) c  | s == '0' = getNumber ss (c*10)
                    | s == '1' = getNumber ss (c*10 +1)
                    | s == '2' = getNumber ss (c*10 +2)
                    | s == '3' = getNumber ss (c*10 +3)
                    | s == '4' = getNumber ss (c*10 +4)
                    | s == '5' = getNumber ss (c*10 +5)
                    | s == '6' = getNumber ss (c*10 +6)
                    | s == '7' = getNumber ss (c*10 +7)
                    | s == '8' = getNumber ss (c*10 +8)
                    | s == '9' = getNumber ss (c*10 +9)
                    | otherwise = getNumber ss c
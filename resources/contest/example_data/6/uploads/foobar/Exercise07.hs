module Exercise07 where

import Control.Monad (replicateM)
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import Data.Sequence (sortBy)

main :: IO ()
main = do
  people <- getLine
  test <- replicateM (read people) getLine
  print (getList test)

getList :: [String] -> [[Int]]
getList xs = [map read (words x) | x <- xs]

getPos :: [[Int]] -> (Int, Int) -> Int
getPos gs (x, y) = gs !! (x -1) !! (y -1)

greeted :: [[Int]] -> (Int, Int) -> Bool
greeted gs (x, y)
  | x == y = True
  | otherwise = gs !! x !! y == 1

addGreet :: [[Int]] -> (Int, Int) -> [[Int]]
addGreet gs (x, y)
  | greeted gs (x, y) = gs
  | otherwise = updateGreetings (updateGreetings gs (x, y)) (y, x)

updateGreetings :: [[Int]] -> (Int, Int) -> [[Int]]
updateGreetings gs (x, y) = take (y -1) gs ++ [take (x -1) (gs !! (y -1)) ++ [1] ++ drop x (gs !! (y -1))] ++ drop (y + 1) gs

getNextMin :: [[Int]] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
getNextMin gs xs used = filter (alreadyUsed used) xs

alreadyUsed :: [(Int, Int)] -> (Int, Int) -> Bool
alreadyUsed ts (x, y) = isJust (find (\(r, s) -> x == r && y == s) ts)
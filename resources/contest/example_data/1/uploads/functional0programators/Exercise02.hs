module Exercise02 where

import Data.List (sortBy, sort)

{-
startupRevenue :: [Int] -> Int
startupRevenue cus = loop (sort cus) 0
  where
    loop [] x = x
    loop (x : xs) y
      | x * (length xs + 1) > y = loop xs (x * (length xs + 1))
      | otherwise = loop xs y
-}
sortRec :: [Int] -> [Int]
sortRec = sortBy (flip compare)


startupRevenue :: [Int] -> Int
startupRevenue xs = startupRevenueHelper sorted sorted 0 0 (-1)
    where sorted = sortRec xs

startupRevenueHelper :: [Int] -> [Int] -> Int -> Int -> Int -> Int
startupRevenueHelper [] _ _ _ m = m
startupRevenueHelper (x:xs) ys c on m | x == on = startupRevenueHelper xs ys c on m
                                      | newMax > m = startupRevenueHelper xs newList newCount x newMax
                                      | otherwise = startupRevenueHelper xs newList newCount x m
                    where (part, newList) = splitAt (countItems ys x) ys
                          newCount = c + length part
                          newMax = newCount * x 

countItems :: [Int] -> Int -> Int 
countItems [] _ = 0
countItems (x:xs) n | x >= n = 1 + countItems xs n 
                    | otherwise = 0

--        remember the numbers you checked

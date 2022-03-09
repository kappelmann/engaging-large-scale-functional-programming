module Exercise02 where

import Data.List

-- startupRevenue :: [Int] -> Int
-- startupRevenue xs = maximum (go (sort xs) [])
--   where
--     go [] acc = acc
--     go li@(y:ys) acc = go ys ((length li * y) : acc)


startupRevenue :: [Int] -> Int
startupRevenue cs =
  let 
    len = length cs
    sortedCs = zip (sort cs) [0..]
  in maximum [c * (len - i) | (c,i) <- sortedCs ]

maximumSelf current [] = current
maximumSelf current (x:xs)
    | current <= x = maximumSelf x xs
    | otherwise = current
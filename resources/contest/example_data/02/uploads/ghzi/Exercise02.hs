module Exercise02 where

import Data.List

startupRevenue :: [Int] -> Int
startupRevenue = startupRevenue2

startupRevenue1 :: [Int] -> Int
startupRevenue1 customers = maximum priceToRevenue
  where
    priceToRevenue = map (\x -> x * length (filter (>=x) customers)) [1 .. 1012]

startupRevenue2 :: [Int] -> Int
startupRevenue2 cs = findMax scs $ length scs
  where
    scs = sort cs

-- custs -> len_custs -> max
findMax :: [Int] -> Int -> Int
findMax [] _ = 0
findMax (p:cs) l = max (p*l) $ findMax cs (l-1)


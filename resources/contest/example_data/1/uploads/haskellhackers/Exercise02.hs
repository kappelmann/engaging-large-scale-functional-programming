module Exercise02 where

import Data.List (nub, sortBy)

-- startupRevenue :: [Int] -> Int
-- startupRevenue offers =  maximum $ map profits (nub offers)
--     where
--         profits x = x * length (filter (>= x) offers)

startupRevenue :: [Int] -> Int
startupRevenue offers = maximum $ aux 0 $ sortBy (flip compare) offers
  where
    aux :: Int -> [Int] -> [Int]
    aux _ [] = []
    aux n (x : xs) = newprice : aux (n + 1 + length same) rest
      where
        (same, rest) = span (== x) xs
        newprice = x * (n + 1 + length same)

-- [1,2,3,4,5,6,7,8]
-- 20

-- [5, 2, 1, 1, 1, 1, 1, 1, 1, 1]
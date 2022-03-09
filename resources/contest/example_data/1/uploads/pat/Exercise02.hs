module Exercise02 where

import Data.List
startupRevenue :: [Int] -> Int
startupRevenue xs = maximum (zipWith (*) sorted [1..])
    where
        sorted = sortBy (flip compare) xs

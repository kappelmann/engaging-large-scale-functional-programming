module Exercise02 where

import Data.List(sort)

startupRevenue :: [Int] -> Int
startupRevenue ns= revenueRec l (sort ns)
    where l = length ns 
    

revenueRec _ [] = 0
revenueRec l (x:xs) = let next = revenueRec (l-1) xs  in
                        if x*l > next then x*l else next
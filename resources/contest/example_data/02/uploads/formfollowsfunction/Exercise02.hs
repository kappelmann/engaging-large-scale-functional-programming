module Exercise02 where

startupRevenue :: [Int] -> Int
startupRevenue xs = maximum [ rev x | x <- xs]
    where rev x = length (filter (>= x) xs) * x

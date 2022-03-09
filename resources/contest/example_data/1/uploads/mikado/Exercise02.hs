module Exercise02 where
import Data.List
import Data.Ord 

startupRevenue :: [Int] -> Int
startupRevenue [] = 0  
startupRevenue xs = go sorted 1
  where 
    sorted = sortOn Down xs
    go [] _ = 0
    go (x:xs) n = max (go xs (n+1)) (n*x)

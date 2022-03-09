module Exercise02 where

import Data.List
import Data.Ord

startupRevenue :: [Int] -> Int
startupRevenue xxs = maximum $ go 1 sorted
  where
    sorted = sortOn Down xxs
    go _   [] = []
    go len (x:xs) = len * x : go (len + 1) xs

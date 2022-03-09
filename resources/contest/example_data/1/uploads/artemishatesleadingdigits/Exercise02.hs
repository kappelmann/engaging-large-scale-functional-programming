module Exercise02 where

import Data.List (sort)

startupRevenue :: [Int] -> Int
startupRevenue ps = maximum simple
  where
    sPs = sort ps
    tw = length ps
    tws = reverse [1 .. tw]
    ptws = zip sPs tws
    simple = map (uncurry (*)) ptws

module Exercise02 where

import Data.List
import Prelude

startupRevenue :: [Int] -> Int
startupRevenue [] = 0
startupRevenue [x] = x
startupRevenue xs = maximum [y * sum (map snd (takeWhile ((>=y) . fst) grouped)) | y <- map fst grouped]
        where grouped = map (\x -> (head x, length x)) $ group $ sortBy (flip compare) xs


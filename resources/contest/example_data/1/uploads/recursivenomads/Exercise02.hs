module Exercise02 where
import Data.List

startupRevenue :: [Int] -> Int
startupRevenue cs = aux (sort cs) (length cs)
    where 
        aux [] _ = 0
        aux (x:xs) l
            | x * l > r = x * l
            | otherwise = r
            where
                r = aux xs (l-1)


-- startupRevenue cs = 2
--     where scs = sort cs
module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (n1, n2, n3, n4) 
    where
        n1 = n `div` (1 + k + k ^ 2 + k ^ 3)
        n2 = k * n1
        n3 = k * n2
        n4 = k * n3


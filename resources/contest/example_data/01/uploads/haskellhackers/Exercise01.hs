module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (a, k*a, k*k*a, k*k*k*a)
    where
        a = n `div` (1 + k + k*k + k*k*k)
module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n,k) = (n1, n1 * k, n1 * k2, n1 * k3)
    where 
        k2 = k * k
        k3 = k2 * k
        n1 = n `div` (1 + k + k2 + k3)



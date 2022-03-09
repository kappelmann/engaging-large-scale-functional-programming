module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n,k) = (n1, n1*k, n1*k*k, n1*k*k*k )
    where n1 = n `div` (1+k+k*k+k*k*k)


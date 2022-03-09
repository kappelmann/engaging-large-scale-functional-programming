module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n,k) = (n1, k*n1, k*k*n1, k*k*k*n1)
    where n1 = div n (1+k+(k*k)+(k*k*k))


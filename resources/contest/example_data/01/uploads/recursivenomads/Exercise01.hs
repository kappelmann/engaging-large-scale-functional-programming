module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (n1,k * n1, k * k * n1, k * k * k * n1) where
    kk = 1 + k + k ^ 2 + k ^ 3
    n1 = n `div` kk


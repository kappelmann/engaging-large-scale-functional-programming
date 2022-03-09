module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = head s
    where s = [(a, k*a, k*k*a, k*k*k*a) | a <- [0..], n == a * (k*k*k + k*k + k + 1)]

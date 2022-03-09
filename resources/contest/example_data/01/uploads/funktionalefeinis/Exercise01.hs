module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (s1, s2, s3, s4)
    where
        s1 = n `div` (1 + k + k ^ 2 + k ^ 3)
        s2 = s1 * k
        s3 = s2 * k
        s4 = s3 * k

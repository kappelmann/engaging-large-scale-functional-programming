module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (n1, n1*k, n1*k^2, n1*k^3)
        where n1 = n `div`(1 +k + k^2 + k^3)


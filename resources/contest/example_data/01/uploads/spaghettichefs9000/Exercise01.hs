module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = let n1 = n `div` (1 + k + k ^ 2 + k ^ 3)
                      in (n1, k * n1, (k ^ 2) * n1, (k ^ 3) * n1)

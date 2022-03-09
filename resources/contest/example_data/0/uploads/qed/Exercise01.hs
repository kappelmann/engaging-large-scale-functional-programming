module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n,k) = (x, x * k, x * k^2, x * k^3)
  where
    x = n `div` (1 + k + k^2 + k^3)
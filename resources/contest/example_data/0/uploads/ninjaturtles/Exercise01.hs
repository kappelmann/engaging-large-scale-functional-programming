module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (mem, k * mem, k ^ 2 * mem, k ^ 3 * mem)
  where
    mem = n `div` (1 + k + k ^ 2 + k ^ 3)

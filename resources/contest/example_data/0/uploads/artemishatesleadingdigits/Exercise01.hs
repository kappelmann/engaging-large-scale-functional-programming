module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (sum, k) = (w, k * w, k * k * w, k * k * k * w)
  where
    i = 1 + k + k * k + k * k * k
    w = sum `div` i

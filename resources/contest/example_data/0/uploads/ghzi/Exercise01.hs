module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (nEins, nEins*k, nEins*(k*k), nEins*(k*k*k))
  where
    kges = 1 + k + (k*k) + (k*k*k)
    nEins = div n kges


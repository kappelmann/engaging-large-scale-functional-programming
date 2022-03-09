module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n,k) = (nOne, k*nOne,k*k*nOne,k*k*k*nOne)
                   where sum = 1 + k + k^2 + k^3
                         nOne = n `div` sum


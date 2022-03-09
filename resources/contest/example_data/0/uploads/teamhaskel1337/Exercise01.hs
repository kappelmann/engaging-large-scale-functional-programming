module Exercise01 where

geometricSumFour k = (k^4 - 1) `div` (k-1)

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n,x) = (n1, n2, n3, n4)
    where gs = geometricSumFour x
          n1 = n `div` gs
          n2 = n1 * x
          n3 = n2 * x
          n4 = n3 * x


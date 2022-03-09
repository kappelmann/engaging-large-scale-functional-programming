module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = (n1,n2,n3,n4)
    where a = 1
          b = a*k
          c = b*k
          d = c*k
          n1 = div n (a+b+c+d)
          n2 = k*n1
          n3 = k*n2
          n4 = k*n3


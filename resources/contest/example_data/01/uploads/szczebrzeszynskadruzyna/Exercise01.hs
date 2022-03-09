module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = let n1 = div n (1+k+k*k+k*k*k) in
				        (n1, n1*k, n1*k*k, n1*k*k*k)


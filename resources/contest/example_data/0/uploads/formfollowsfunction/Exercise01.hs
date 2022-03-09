module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (noStudents, k) = (n1,n2,n3,n4)
                                where
                                    n1 = noStudents `div` (1 + k  + (k^2) + (k^3))
                                    n2 = n1 * k
                                    n3 = n2 * k
                                    n4 = n3 * k

-- n = n1 + kn1 + k^2*n1 + k^3 * n1

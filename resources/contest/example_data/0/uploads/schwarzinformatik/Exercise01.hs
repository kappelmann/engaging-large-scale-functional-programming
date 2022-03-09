module Exercise01 where

    -- n = n1 + kn1 + kn2 + kn3
    -- n = n1 + kn1 + kkn1 + kkkn1
    -- n
    
    -- n - kn2 + kn3 = n1 + kn1

    -- n/k = n1/k + n1 + n2 + n3

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n,k) = (n1, k*n1, k*k*n1, k*k*k*n1)
    where
        n1 = bruteForce (n, k) 0 n (div n 2)

bruteForce :: (Integer, Integer) -> Integer -> Integer -> Integer -> Integer
bruteForce (n, k) uS oS n1
    | n1 + n2 + n3 + n4 == n = n1
    | n1 + n2 + n3 + n4 > n = bruteForce (n,k) uS n1 (div (uS + n1) 2)
    | n1 + n2 + n3 + n4 < n = bruteForce (n,k) n1 oS (div (n1 + oS) 2)
        where
            n2 = k * n1
            n3 = k * n2
            n4 = k * n3


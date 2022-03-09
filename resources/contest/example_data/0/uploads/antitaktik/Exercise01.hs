module Exercise01 where
type Quad = (Integer, Integer, Integer, Integer)

contestTeams :: (Integer, Integer) -> Quad
contestTeams (n, k) = (v0, v0*k, v0*k*k, v0*k*k*k)
    -- head $ filter (`sumTuple` n) (allContests k)
    where
        x = n `div` (k^3 + k^2 + k + 1)
        k3 = k^3
        stuff = 1 + k + k^2 + k^3
        v0 = n `div` stuff

contestC :: (Integer, Integer) -> Quad
contestC (n, k) = (n1, n1 *k, n1*k^2, n1*k^3)
    where n1 = head $filter (\x -> x + x*k + x*k^2 + x*k^3 == n)[1..]

allContests :: Integer -> [Quad]
allContests k = [(n1, n1 *k, n1*k^2, n1*k^3) | n1 <- [1..]]

sumTuple :: Quad -> Integer -> Bool 
sumTuple (a,b,c,d) n = a+b+c+d == n

contestB :: Integer -> Integer -> Integer -> Integer -> Quad
contestB stepv3 basev3 n k = 
    if (k > 1 && stepv3 + x * 2 >= n) || (k == n && undefined )
     then (stepv3 `div`(k^3),stepv3 `div`(k^2),stepv3 `div` k,stepv3) 
     else contestB (stepv3 + basev3) basev3 n k
    where x = stepv3 `div` k
          y = x `div` k
          z = y `div` k


contestA :: Quad -> Quad -> Integer -> Quad
contestA step@(v0,v1,v2,v3) base n = if 2*v3 >= n then step else contestA (add step base) base n

add :: Quad -> Quad -> Quad
add (a,b,c,d) (e,f,g,h) = (a+e, b+f, c+g, d+h)

test :: [Int] -> Int 
test xs = aux (minimum xs) xs
    where
        aux :: Int -> [Int] -> Int
        aux min ys
            | sum (filtered min ys) < sum (filtered (min+1) ys) = aux (min+1) ys
            | otherwise = sum (filtered min ys)
        filtered :: Int -> [Int] -> [Int]
        filtered min ys = filter (>= min) ys
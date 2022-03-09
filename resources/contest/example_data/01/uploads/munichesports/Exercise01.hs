module Exercise01 where

contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
contestTeams (n, k) = let uno = n `div` (k^3 + k^2 + k + 1) in ( uno, k*uno, k^2*uno, k^3*uno)

-- contestTeams :: (Integer, Integer) -> (Integer, Integer, Integer, Integer)
-- contestTeams (n, k) = contestTeams1 (fromIntegral n, fromIntegral k)

--contestTeams1 :: (RealFrac d) => (d, d) -> (Integer, Integer, Integer, Integer)
--contestTeams1 (n,k) = let uno = 1 / ((k^3 + k^2 + k + 1) / n) in (roundToInt uno, roundToInt $  k*uno,roundToInt $ k^2*uno,roundToInt $ k^3*uno)

-- roundToInt :: (RealFrac a, Num b) => 'a -> b
-- roundToInt x = fromIntegral $ round x'

-- 0 = k^3n1 + k^2n1 + kn1 + n1 - 40
-- n = (k^3 + k^2 + k + 1) n1
-- n/n1 = (k^3 + k^2 + k + 1)
-- n/n1 / (n/1) = (k^3 + k^2 + k + 1) / n 
-- n/n1 * (1/n) = (k^3 + k^2 + k + 1) / n 
-- (1) / (n1) = (k^3 + k^2 + k + 1) / n 
-- n1 = 1 / ....
-- 1/(x/n) 


-- 1/n1* n = 1 / ((k³ + k² + k + 1) 
-- n1 = 1 / ((k³ + k² + k + 1) / n


test :: Integer -> [(Integer, Integer, Integer, Integer)]
test x = contestTeams (kToN x,x) : test (x+1)

kToN :: Integer -> Integer
kToN x = k^3 + k^2 + k + 1
  where k = x

kToN1 :: Integer -> Integer -> Integer
kToN1 x y = k^3*y + k^2*y + k*y + y
  where k = x

test1 x y = contestTeams (kToN1 x y,x)
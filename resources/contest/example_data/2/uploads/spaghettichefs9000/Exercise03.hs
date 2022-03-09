module Exercise03 where

primeMayor :: Integer -> Integer
primeMayor n
  | n == 2 = 1
  | even n = 2
  | otherwise = nonEvenTax n

-- From Haskell wiki:
primes :: [Integer]
-- primes = 2 : 3 : sieve (tail primes) [5,7..]
--    where
--        sieve (p:ps) xs = h ++ sieve ps [x | x<- t, x `rem` p /= 0]
--            where (h,~(_:t)) = span (< p * p) xs
-- primes = 2 : 3 : sieve 0 (tail primes) 3

-- sieve k (p:ps) x = [n | n <- [x+2,x+4..p*p-2], and [n `rem` p /= 0| p <- fs]] ++ sieve (k+1) ps (p*p)
--     where fs = take k (tail primes)

primes = 2 : 3 : sieve [] (tail primes) 3
    where
        sieve fs (p:ps) x = [x+2,x+4..q-2] `minus` foldl union [] mults ++ sieve fs' ps q
            where
                q     = p * p
                mults = [[y+s,y+2*s..q] | (s,y) <- fs]
                fs'   = (2*p,q) : zip (map fst fs) (map last mults)

union :: Ord a => [a] -> [a] -> [a]
union [] ys = ys
union xs [] = xs
union (x:xs) (y:ys)
  | x < y     = x : union xs (y:ys)
  | y < x     = y : union (x:xs) ys
  | otherwise = x : union xs ys

minus :: Ord a => [a] -> [a] -> [a]
minus [] _  = []
minus xs [] = xs
minus (x:xs) (y:ys)
  | x < y     = x : minus xs (y:ys)
  | y < x     = minus (x:xs) ys
  | otherwise = minus xs (y:ys)

nonEvenTax :: Integer -> Integer
nonEvenTax n = let (l:x:_) = dropWhile (< n - 2) primes
               in if l == n || x == n then 1 else if (n - 2) == l then 2 else 3

module Exercise03 where

primeMayor :: Integer -> Integer
primeMayor a
	| a == 0 = 1
	| a == 1 = 1
	| prime (fromIntegral a) = 1
	| even a = 2
	| otherwise = primeMayorOdd a

-- ~ elemS x (y:ys)
	-- ~ | x == y = True
	-- ~ | x < y = False
	-- ~ | otherwise = elemS x ys

-- ~ primes :: [Int]
-- ~ primes = sieve [2..]

-- ~ sieve :: [Int] -> [Int]
-- ~ sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = 2:3:5:7:11:13:17: filter prime [19..]

prime :: Int -> Bool
prime 0 = False
prime 1 = False
prime x = and [x `mod` p  /= 0 | p <- takeWhile (<=sqx) primes]
	where sqx = floor $ sqrt $ fromIntegral x

prime2 :: Int -> Bool
prime2 x = head (dropWhile (<x) primes) == x

-- ~ primSums :: [Int]
-- ~ primSums = merge [[p1 + p2 | p2 <- primes] | p1 <- primes]

primeMayorOdd :: Integer -> Integer
primeMayorOdd abig
	| prime (a-2) = 2
	| otherwise = 3
	where
		-- ~ zerlegs = [(p, a-p) | p <- takeWhile (<=ah) primes, prime2 (a-p)]
		-- ~ ah = a `div` 2
		a = fromIntegral abig

-- ~ primeMayorOdd' a
	-- ~ | null zerlegs = (1,1)
	-- ~ | otherwise = head zerlegs
	-- ~ where
		-- ~ zerlegs = [(p, a-p) | p <- takeWhile (<=ah) primes, prime (a-p)]
		-- ~ ah = a `div` 2

module Exercise04 where

rudolph :: String -> (Int, Int)
rudolph s = if null t then (0, 1) else (maxi * 2, times)
  where
    t = cd s (0, 0)
    maxi :: Int
    maxi = maximum [l | (l, r) <- t]
    times = length [l | (l, r) <- t, l == maxi]

cd :: String -> (Int, Int) -> [(Int, Int)]
cd [] (l, r) = []
cd ('R' : xs) (l, r)
  | l == r + 1 = (l, r + 1) : next
  | otherwise = next
  where
    next = cd xs (l, r + 1)
cd ('L' : xs) (l, r)
  | l + 1 > r = cd xs (0, 0) -- fail   neu anfangen
  | l + 1 == r = (l + 1, r) : next
  | otherwise = next
  where
    next = cd xs (l + 1, r)

test1 = cd "LRRRLLLLRRLRLL" (0, 0)

test2 = cd "LR" (0, 0)

test3 = rudolph "LRRRLLLLRRLRLL"

test4 = rudolph "LR"

test5 = rudolph "RLLR"
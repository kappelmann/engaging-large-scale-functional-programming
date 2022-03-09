module Exercise02 where
import Data.List
--if length xs > 10000 || any (\x -> x < 1 || x > 1012 ) xs then -1 else 
startupRevenue :: [Int] -> Int
startupRevenue [] = 0
startupRevenue xs = maximum $ h better_xs
  where
    better_xs = nub xs
    len = length xs
    h [] = []
    h (y:ys) = rev: h ys
      where
        rev = getRevenue y xs
    a = avg xs  


t :: [Int]
t = 1:t 


-- getRevenue :: Int -> [Int] -> Int
-- getRevenue a xs = length ys * a
--   where ys = filter (>=a) xs

getRevenue :: Int -> [Int] -> Int
getRevenue a xs = h xs * a
  where
    h [] = 0
    h (x:xs)
      | x >= a = 1 + h xs
      | otherwise = h xs


avg :: [Int] -> Int
avg xs = sum xs `div` length xs








-- min ai euros for product
-- m euros price = m * number of people willing to pay m euros OR MORE

-- Constraints: 1 <= n <= 10000
-- Constraints: 1 <= ai <= 1012


-- Simplexx
-- max  (a * i) * x 
-- s.t. a <= 1000
--      a >= 1
--      n <= 1012
--      n >= 1
--      i <= ai     wobei a E [1...i]
--   n - i = x
--      x >= 0

module Exercise02 where

import Data.List
import Data.Maybe

startupRevenue :: [Int] -> Int
startupRevenue as = startupRevenue' (sort as) (nub as) 0

startupRevenue' as [] r = r
startupRevenue' as (p:ps) r = startupRevenue' as ps (max r rNew)
    where rNew = p * count as p 0

count [] p n = n
count (a:as) p n
    | p > a = count as p n
    | otherwise = count as p (n+1)

--Versuch 1:
--startupRevenue :: [Int] -> Int
--startupRevenue as = maximum [revenue m | m <- nub as]
--    where revenue x = sum [if a >= x then x else 0 | a <- as]

--Versuch 2:
--startupRevenue :: [Int] -> Int
--startupRevenue as = startupRevenue' as (sort (nub as)) 0

--startupRevenue' as [] n = n
--startupRevenue' as (m:ms) n = if revenue < n then n else startupRevenue' as ms revenue
--    where revenue = m * length [a | a <- as, a >= m] 

--startupRevenueSohrab :: [Int] -> Int
--startupRevenueSohrab as = revenue
--    where 
--        potenzmenge = subsequences as
--        sumSublists = map sum potenzmenge
--        maximumSum = maximum sumSublists
--        index = elemIndex maximumSum sumSublists
--        m = minimum (potenzmenge !! fromJust index)
--        revenue = m * length (filter (>= m) as)

module Exercise02 where

--import Debug.Trace
import Data.List

startupRevenue :: [Int] -> Int
--startupRevenue xs = maximum $ foldRev xs (minimum xs) []
startupRevenue = test3
 

sorting :: [Int] -> Int
sorting xs = max (auxSort sorted xs) (auxSort (reverse sorted) xs)
    where sorted = sort xs

auxSort :: [Int] -> [Int] -> Int
auxSort [] zs = 0
auxSort (x:y:ys) zs
    | x == y = auxSort (y:ys) zs
    | money x < money y  = auxSort (y:ys) zs
    | otherwise = money x
    where
     money c = c * length (filtered c) 
     filtered max = filter (>= max) zs
auxSort [x] ys = x * length (filter (>= x) ys)


test2 :: [Int] -> Int 
test2 xs = maximum (alltests xs)

alltests :: [Int] -> [Int]
alltests xs = map (\x -> x * length (filter (>=x) xs)) (nub xs) 

test3 :: [Int] -> Int
test3 xs = foldl (\acc x -> max acc (x * length (filter (>=x) xs))) 0 (nub xs)

-- minMaxProbing :: [Int] -> Int
-- minMaxProbing xs = test' xs (test xs) 

test :: [Int] -> Int 
test [] = 0
test xs = aux (minimum xs) xs

--[2,2,3,10]
aux :: Int -> [Int] -> Int
aux min ys
    -- | trace (show min) False = undefined 
    | any (> min) ys && money min < money min2 = aux min2 ys
    | otherwise = money min
    where
     min2 = minimum $ filter (>min) ys
     filtered min = filter (>= min) ys
     money min = min * length (filtered min) 

-- test' :: [Int] -> Int -> Int 
-- test' [] x = x
-- test' xs x = aux (maximum xs) x xs

--[2,2,3,10]
aux' :: Int -> Int -> [Int] -> Int
aux' max max' ys
    -- | trace (show min) False = undefined 
    | any (< max) ys && money max > money max2 = aux max2 ys
    | otherwise = money max
    where
     max2 = maximum $ filter (<max) ys
     money max = max * length (filtered max) 
     filtered max = filter (>= max) ys

-- --test [2,4,2,5,200]
-- testfold :: (Int, Int)
-- testfold = foldRev xs (minimum xs) 0
--     where xs = [2,4,2,200,2]

foldRev :: [Int] -> Int -> [Int] -> [Int]
foldRev [] _ prev = prev
foldRev xs bar prev = if null newxs || null filt 
                            then prev
                            else foldRev newxs (minimum filt) (sum:prev)
    where (sum, newxs, count) = foldl (\acc@(sum, newxs, count) x -> if x < bar then acc else (sum+bar, x:xs, count+1)) (0, [], 0) xs
          filt = filter (>bar) xs

foldRev2 :: [Int] -> Int -> [Int] -> [Int]
foldRev2 [] _ prev = prev
foldRev2 xs bar prev = if null newxs || null filt 
                            then prev
                            else foldRev newxs (minimum filt) (sum:prev)
    where newxs = filter (>= bar) xs
          filt = filter (>bar) newxs
          sum = length newxs * bar

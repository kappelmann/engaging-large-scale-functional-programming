module Exercise02 where
import Data.List

startupRevenue :: [Int] -> Int
startupRevenue xs = startHelper xs $ sort $ nub xs

startHelper:: [Int]->[Int]->Int 
startHelper xs ys = fst $ foldl (\(cm, cxs) x ->let (len, filtered)=filterGreaterLength xs x in (if x * len > cm then x * len else cm, filtered)) (0,xs) ys


filterGreaterLength :: [Int] -> Int -> (Int,[Int])
filterGreaterLength [] _ = (0, [])
filterGreaterLength (x:xs) y
    | x >= y = (1+len, x:filtered)
    | otherwise = (len, filtered)
    where (len,filtered) = filterGreaterLength xs y
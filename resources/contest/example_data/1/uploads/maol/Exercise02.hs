module Exercise02 where

startupRevenue' :: [Int] -> Int
startupRevenue' ns = 
    let sorted = mergeSort id ns
        maxPrice = maximum sorted
        minPrice = minimum sorted
    in  foldr (\x acc -> max (x * length [customer | customer <- sorted, customer >= x]) acc) 0 [minPrice..maxPrice]

startupRevenue :: [Int] -> Int
startupRevenue cs =
    let sorted = reverse $ mergeSort id cs
        maxPrice = maximum sorted
        minPrice = minimum sorted
    in startupRevenueH sorted 0 0 maxPrice minPrice

startupRevenueH :: [Int] -> Int -> Int -> Int -> Int -> Int
startupRevenueH [] currentMax length currentPrice _ = max currentMax (length * currentPrice)
startupRevenueH (c:cs) currentMax length currentPrice minPrice
    | c >= currentPrice         = startupRevenueH cs currentMax (length + 1) currentPrice minPrice
    | otherwise                 = startupRevenueH (c:cs) (max revAtPrice currentMax) length (currentPrice - 1) minPrice
    where revAtPrice = length * currentPrice

mergeSort :: (Ord b) => (a -> b) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort f xs =
    let len = length xs
        middle = div len 2
        (left, right) = splitAt middle xs
    in merge f (mergeSort f left) (mergeSort f right)

merge :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
merge _ [] [] = []
merge _ [] ys = ys
merge _ xs [] = xs
merge f (x:xs) (y:ys)
    | f x < f y = x : merge f xs (y:ys)
    | otherwise = y : merge f (x:xs) ys
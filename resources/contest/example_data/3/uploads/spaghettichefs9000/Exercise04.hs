module Exercise04 where

rudolph :: String -> (Int, Int) 
-- rudolph s ? 
-- rudolph s = walkOutside s (0,1)
rudolph s = sumResults (rudolphHelper s)

-- rudolphHelper :: String -> [(Int,Int)]
-- rudolphHelper [] = []
-- rudolphHelper (x:xs) = walkOutside (x:xs) (0,1) : rudolphHelper xs

sumResults :: [(Int,Int)] -> (Int,Int)
sumResults xs = (m, sum (map snd (filter (\(maxL, n) -> maxL == m) xs)))
    where m = findMax xs

findMax :: [(Int,Int)] -> Int
findMax xs = maximum (map fst xs)

-- find paths from all starting points
-- combine results

rudolphHelper :: String -> [(Int,Int)]
rudolphHelper [] = [(0,1)]
rudolphHelper s
    | nextResult /= (0,1) = nextResult : rudolphHelper (drop (fst nextResult) s)
    | otherwise  = rudolphHelper (tail s)
    where nextResult = walkOutside s (0,1)
        -- restString = 

-- walkOutside s (0,1)

-- walkInside :: String -> Int -> Int -> (Int,Int) -> (Int,Int)
-- walkInside [] pos l res
--     | pos == 0 = updateResult res l
--     | otherwise = ----- heeere
-- walkInside (x:xs) pos l res
--     | pos == 0 && x == 'L' = walkOutside xs (updateResult res l)
--     -- | x == 'L' && pos == 0 = walkOutside xs (updateResult res l)
--     | x == 'L' = walkInside xs (pos -1) (l+1) res
--     | otherwise = walkInside xs (pos + 1) (l+1) res


walkInside :: String -> Int -> Int -> Int -> (Int,Int) -> (Int,Int)
walkInside [] pos l zeroAfter res
    | pos == 0 = updateResult res l
    | otherwise = updateResult res (l - (l-zeroAfter))
    -- | otherwise = (l,zeroAfter)
walkInside (x:xs) pos l zeroAfter res
    -- | pos == 0 && x == 'L' = walkOutside xs (updateResult res l)
    | pos == 0 && x == 'L' = updateResult res l

    | pos == 0 && x == 'R' = walkInside xs (pos + 1) (l+1) l res
    | x == 'L' = walkInside xs (pos -1) (l+1) zeroAfter res
    | otherwise = walkInside xs (pos + 1) (l+1) zeroAfter res

updateResult :: (Int,Int) -> Int -> (Int, Int)
updateResult (maxL, num) curL
    | curL == 0 = (maxL, num)
    | curL > maxL = (curL,1)
    | curL == maxL = (maxL, num +1)
    | otherwise = (maxL, num)

walkOutside :: String -> (Int,Int) -> (Int,Int)
walkOutside [] res = res
walkOutside (x:xs) res
    | x == 'R' = walkInside xs 1 1 0 res 
    | otherwise = walkOutside xs res 
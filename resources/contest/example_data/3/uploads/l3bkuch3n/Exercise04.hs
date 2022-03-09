module Exercise04 where

rudolph :: String -> (Int, Int) 
rudolph [] = (0, 1)
rudolph (x:xs) 
    | 'L' == x = rudolph xs
    | otherwise = help (x:xs) 0 0 0 0

help :: String -> Int -> Int -> Int -> Int -> (Int, Int)
help [] _ _ mx num 
    | mx == 0 = (0, 1)
    | otherwise = (mx, num)
help (x:xs) i len mx num
    | x == 'R'  = help xs (i+1) (len+1)  mx                 num
    | i == 0    = help xs 0      0       (max mx len)       (if mx == len then num + 1 else (if len > mx then 1 else num))
    | i == 1    = help xs 0     (len+1)  (max mx (len + 1)) (if mx == (len + 1) then num + 1 else (if (len + 1) > mx then 1 else num))
    | otherwise = help xs (i-1) (len+1)  mx                 num




rudolph2 :: String -> (Int, Int) 
rudolph2 [] = (0, 1)
rudolph2 (x:xs) 
    | 'L' == x = rudolph xs
    | otherwise = th (x:xs) 0 0 0 0

th :: String -> Int -> Int -> Int -> Int -> (Int,Int)
th [] _ l _ c = (l, c)
th (x:xs) home longestPath currentPathLength count | x == 'R'      = th xs (home + 1) longestPath (currentPathLength + 1) count
                                                   | home - 1 == 0 = if currentPathLength + 1 > longestPath then th xs 0 (currentPathLength + 1) 0 1
                                                                                                            else if currentPathLength + 1 < longestPath then th xs 0 longestPath 0 count
                                                                                                            else th xs 0 longestPath 0 (count + 1)
                                                   | otherwise     =  th xs (home - 1) longestPath (currentPathLength + 1) count
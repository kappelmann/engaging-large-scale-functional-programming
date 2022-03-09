module Exercise04 where

rudolph :: String -> (Int, Int) 
rudolph [] = (0,1)
rudolph xs =helper xs 0 0 (0,1) 

helper :: String -> Int -> Int ->(Int, Int)-> (Int, Int)
helper [] pos count (a,b)

    | pos == 0 && count>=1 =  if count>a then (count,1) else if count==a then  (a,b+1) else (a,b)
    | otherwise = (a,b)
helper (x:xs) pos count (a,b)
    | x /= 'L' && x /= 'R' = helper xs 0 0 (a,b)
    | pos == 0 && count >=1 = if count>a then helper (x:xs) 0 0 (count,1) else if count==a then helper (x:xs) 0 0 (a,b+1) else helper (x:xs) 0 0 (a,b)
    | pos == 0 && x == 'L' = helper xs 0 0 (a,b)
    | x == 'R' = helper xs (pos+1) (count+1) (a,b)
    | otherwise = helper xs (pos-1) (count+1) (a,b)

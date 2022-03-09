module Exercise04 where

rudolph :: String -> (Int, Int)
rudolph s 
    | ret == (0,0) = (0,1)
    | otherwise = ret
    where ret = rudolphHelp s 0 0 0 0

-- rest of string, longest, number, position, current count
rudolphHelp :: String -> Int -> Int -> Int -> Int -> (Int ,Int )
rudolphHelp [] l n p i
    | p == 0 && i > l = (i,1)
    | p == 0 && l == i = (l,n+1)
    | otherwise  = (l,n)
--rudolphHelp ('L':xs) l n 0 _ = rudolphHelp xs l n 0 0 -- case still at home
rudolphHelp ('L':xs) l n 0 0 = rudolphHelp xs l n 0 0
rudolphHelp ('L':xs) l n 0 i 
    | l > i = rudolphHelp xs l n 0 0
    | l == i = rudolphHelp xs  l (n+1) 0 0 
    | otherwise = rudolphHelp xs i 1 0 0
rudolphHelp ('L':xs) l n p i = rudolphHelp xs l n (p-1) (i+1) -- move left
rudolphHelp ('R':xs) l n p i = rudolphHelp xs l n (p+1) (i+1) -- move righ
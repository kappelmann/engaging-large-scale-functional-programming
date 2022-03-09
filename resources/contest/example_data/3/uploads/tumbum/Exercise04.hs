module Exercise04 where

rudolph :: String -> (Int,Int)
rudolph "" = (0,1)
rudolph s = case x of
                [] -> (0,1)
                _ -> (z, length $ filter (\y -> z == length y) x)
            where x = filter isValid (substrings s)
                  z = maximum $ map length x

substrings :: String -> [String]
substrings s = [ take x $ drop y s | y <- [0..length s], x <- [2 .. length s], even x, y + x <= length s]

isValid :: String -> Bool
isValid [] = True
isValid s@(x:_)
    | x == 'L' = False
    | otherwise = inBounds s 0


inBounds :: String -> Int -> Bool
inBounds [] c  = c == 0
inBounds (x:xs) c
    | c < 0 = False
    | x == 'L'  = inBounds xs (c-1)
    | otherwise = inBounds xs (c+1)
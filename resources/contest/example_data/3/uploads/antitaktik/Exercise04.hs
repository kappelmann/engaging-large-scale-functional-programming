module Exercise04 where



rudolph :: String -> (Int, Int)
rudolph s = elf (0,1) s 0 0

-- coins RLR = 1 (+1-1+1)
elf :: (Int,Int) -> String -> Int -> Int -> (Int,Int)
elf best@(lenB,countB) [] len coins = if len == lenB && len /= 0 && coins == 0 then (lenB,countB+1) else best
elf best ('L':s) 0 _ = elf best s 0 0
elf best@(lenB,countB) (c:s) len coins 
    | c == 'L' && coins == 0 && len > lenB = elf (len, 1) s 0 0             --new best
    | c == 'L' && coins == 0 && len == lenB = elf (lenB, countB+1) s 0 0    --another best
    | c == 'L' = elf best s (len+1) (coins-1)
    | c == 'R' = elf best s (len+1) (coins+1)
    | otherwise = error "christmas miracle"

-- RLL
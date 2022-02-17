module Exercise05 where

-- May or may not be useful: computes the logarithm base 2 (rounded down) of the given number.
log2 :: (Integral a, Num b) => a -> b
log2 = let go acc n = if n <= 1 then acc else go (acc + 1) (n `div` 2) in go 0

-- For a detailed explanation of why this works, see the upcoming Wettbewerb blog entry
{-WETT-}
decompose :: [Integer] -> [Integer]
decompose [] = []
decompose ds = if 0 `elem` ds then [] else nCubes : decompose ds'
  where ds'    = map (`div` 2) ds
        nCubes = product ds - 2 ^ length ds * product ds'
{-TTEW-}


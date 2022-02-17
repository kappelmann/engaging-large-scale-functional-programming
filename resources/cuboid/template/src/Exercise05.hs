module Exercise05 where

-- May or may not be useful: computes the logarithm base 2 (rounded down) of the given number.
-- You don't have to move this into the WETT tags if you want to use it.
log2 :: (Integral a, Num b) => a -> b
log2 = let go acc n = if n <= 1 then acc else go (acc + 1) (n `div` 2) in go 0

{-WETT-}
decompose :: [Integer] -> [Integer]
decompose ds = undefined
{-TTEW-}


module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen

type Number = Int

data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = test xs 0

test :: [Either Colour Number] -> Integer -> Integer
test ((Left Gold) : xs) k = test xs (k + 1)
test (Right n : xs) k = if even n then test xs (k+1) else test xs k
test (x : xs) k =test xs k
test [] k = k
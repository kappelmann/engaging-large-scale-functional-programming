module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

shouldCount :: Either Colour Number -> Integer
shouldCount (Left Gold) = 1
shouldCount (Right x) | even x = 1
shouldCount _ = 0

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee = foldl (\x c -> x + shouldCount c) 0
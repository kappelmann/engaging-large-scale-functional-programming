module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = sum [1 :: Integer | x <- xs, mustBeFlipped x]

mustBeFlipped :: Either Colour Number -> Bool
mustBeFlipped card = either (\x -> isGold x) (\y -> even y) card

isGold :: Colour -> Bool
isGold Gold = True
isGold RustyBrown = False
isGold TurtleGreen = False
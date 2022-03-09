module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee [] = 0
tweedledumTweedledee ((Left Gold):deck) = 1 + tweedledumTweedledee deck
tweedledumTweedledee ((Right num):deck)
    | even num = 1 + tweedledumTweedledee deck
tweedledumTweedledee (_:deck) = tweedledumTweedledee deck

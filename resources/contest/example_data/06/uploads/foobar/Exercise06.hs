module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee [] = 0
tweedledumTweedledee ((Left Gold):xs) = 1 + tweedledumTweedledee xs
tweedledumTweedledee ((Left _):xs) = tweedledumTweedledee xs
tweedledumTweedledee ((Right n):xs) = if even n then 1 + tweedledumTweedledee xs else tweedledumTweedledee xs


module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee [] = 0
tweedledumTweedledee ((Left Gold):xs) = 1 + tweedledumTweedledee xs
tweedledumTweedledee ((Right x):xs)
    | even x = 1 + tweedledumTweedledee xs
    | otherwise = tweedledumTweedledee xs
tweedledumTweedledee (x:xs) = tweedledumTweedledee xs

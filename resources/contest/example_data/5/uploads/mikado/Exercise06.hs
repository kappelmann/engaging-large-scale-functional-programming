module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee [] = 0
tweedledumTweedledee (either:xs) = case either of
    Left Gold -> 1 + tweedledumTweedledee xs
    Right x -> if even x then 1 + tweedledumTweedledee xs else tweedledumTweedledee xs
    _ -> tweedledumTweedledee xs
    
module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee cards = tweedledumTweedledeeAux cards 0

tweedledumTweedledeeAux :: [Either Colour Number]-> Integer -> Integer
tweedledumTweedledeeAux []         x = x
tweedledumTweedledeeAux ((Left Gold):xs)         x = tweedledumTweedledeeAux xs (x+1)
tweedledumTweedledeeAux ((Left RustyBrown):xs)   x = tweedledumTweedledeeAux xs x
tweedledumTweedledeeAux ((Left TurtleGreen):xs)  x = tweedledumTweedledeeAux xs x
tweedledumTweedledeeAux ((Right n):xs)  x = val
            where 
                val = if odd  n then tweedledumTweedledeeAux xs x else tweedledumTweedledeeAux xs (x+1) 
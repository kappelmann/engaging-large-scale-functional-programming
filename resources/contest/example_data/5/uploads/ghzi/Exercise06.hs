module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = toInteger $ length $ filter haveTurn xs
    where haveTurn (Left Gold) = True
          haveTurn (Right n) = even n
          haveTurn _ = False

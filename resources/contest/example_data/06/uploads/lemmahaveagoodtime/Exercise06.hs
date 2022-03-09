module Exercise06 where

import Data.Either

data Colour = Gold | RustyBrown | TurtleGreen
    deriving Eq 
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = toInteger (length [x | x <- xs, fromLeft RustyBrown x == Gold || even(fromRight 3 x)])

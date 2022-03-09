module Exercise06 where

import Data.Either (lefts,rights)

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = toInteger $ length [ x | x <- lefts xs, isGold x] + length [ x | x <- rights xs, even x]

isGold :: Colour -> Bool
isGold Gold = True
isGold _ = False
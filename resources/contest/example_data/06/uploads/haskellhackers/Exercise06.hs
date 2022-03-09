
module Exercise06 where

import Data.Either

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = toInteger (goldHearts + evenNumbers)
    where
        goldHearts = length [x | x <- xs, isGold x]
        evenNumbers = length [x | x <- xs, isEven x]

isGold :: Either Colour Number -> Bool
isGold (Left Gold) = True
isGold _ = False

isEven :: Either Colour Number -> Bool
isEven (Right n) = even n
isEven _ = False

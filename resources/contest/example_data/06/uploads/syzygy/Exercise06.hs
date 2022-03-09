module Exercise06 where

import Data.Either (partitionEithers)

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = checkNumbers numbers + checkHearts colors
    where (colors, numbers) = partitionEithers xs

checkNumbers :: [Number] -> Integer
checkNumbers [] = 0
checkNumbers (x:xs)
    | even x = 1 + checkNumbers xs
    | otherwise = checkNumbers xs

checkHearts :: [Colour] -> Integer
checkHearts [] = 0
checkHearts (Gold:xs) = 1 + checkHearts xs
checkHearts (_:xs) = checkHearts xs

module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee = tweedledumTweedledee' 0

tweedledumTweedledee' :: Integer -> [Either Colour Number] -> Integer
tweedledumTweedledee' i [] = i
tweedledumTweedledee' i ((Left Gold):xs) = tweedledumTweedledee' (i+1) xs
tweedledumTweedledee' i ((Right x):xs)
    | even x = tweedledumTweedledee' (i+1) xs
    | otherwise = tweedledumTweedledee' i xs
tweedledumTweedledee' i (_:xs) = tweedledumTweedledee' i xs

module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
--all even numbers + all golden hearts
tweedledumTweedledee = tweedledumTweedledee' 0

tweedledumTweedledee' acc [] = acc
tweedledumTweedledee' acc (x:xs) = tweedledumTweedledee' (acc + i) xs
  where i = case x of
              Left Gold -> 1
              Right i -> if even i then 1 else 0
              otherwise -> 0

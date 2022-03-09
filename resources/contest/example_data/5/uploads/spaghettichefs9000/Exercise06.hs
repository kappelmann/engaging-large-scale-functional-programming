module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = aux xs 0
    where aux [] acc = acc
          aux ((Left Gold) : xs) acc = aux xs (acc + 1)
          aux ((Right num) : xs) acc = if   even num 
                                       then aux xs (acc + 1)
                                       else aux xs acc
          aux (x : xs) acc = aux xs acc

module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = helpTLT xs 0



helpTLT :: [Either Colour Number] -> Integer -> Integer
helpTLT [] acc = acc
helpTLT ((Left Gold) : xs) acc = helpTLT xs (acc+1)
helpTLT ((Right i) : xs) acc = if i `mod` 2 == 0 then helpTLT xs (acc+1) else helpTLT xs (acc)
helpTLT ((Left RustyBrown ) : xs) acc = helpTLT xs (acc)
helpTLT ((Left TurtleGreen ) : xs) acc = helpTLT xs (acc)
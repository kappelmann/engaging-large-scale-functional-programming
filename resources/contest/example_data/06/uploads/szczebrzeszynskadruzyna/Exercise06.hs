module Exercise06 where



data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = recFind xs 0

recFind [] n = n
recFind (Left Gold:xs) n =  recFind xs (n+1)
recFind (Right x:xs) n = if even x then recFind xs (n+1) else recFind xs n
recFind (x:xs) n = recFind xs n
module Exercise06 where
import Data.Either

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tt :: [Either Colour Number] -> Integer -> Integer
tt [] n = n
tt ((Left Gold):cs) n = tt cs (n+1)
tt ((Right num):cs) n = if even num then tt cs (n+1) else tt cs n
tt (_:cs) n = tt cs n

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee cs = tt cs 0

module Exercise06 where
import Prelude 
import Data.List
import Data.Either
import Data.Eq

data Colour = Gold | RustyBrown | TurtleGreen deriving (Eq)
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee xs = genericLength [x|x<- rights xs, even x] + genericLength [x|x<- lefts xs, x == Gold]

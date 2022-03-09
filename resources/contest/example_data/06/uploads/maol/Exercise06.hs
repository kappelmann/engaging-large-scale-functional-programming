module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee cards = tweedledumTweedledeeH cards 0

tweedledumTweedledeeH :: [Either Colour Number] -> Integer -> Integer
tweedledumTweedledeeH [] noOfCards = noOfCards
tweedledumTweedledeeH [x] noOfCards = either colorAux numberAux x
    where colorAux x = singleColourCard x noOfCards
          numberAux x = singleNumberCard x noOfCards
tweedledumTweedledeeH (x : y : cs) noOfCards = either colorAux numberAux x
    where colorAux x = colourCard x (y : cs) noOfCards
          numberAux x = numberCard x (y : cs) noOfCards

singleColourCard :: Colour -> Integer -> Integer
singleColourCard Gold noOfCards = noOfCards + 1
singleColourCard _ noOfCards = noOfCards

colourCard :: Colour -> [Either Colour Number] -> Integer -> Integer
colourCard Gold (y : cs) noOfCards = tweedledumTweedledeeH (y : cs) (noOfCards + 1)
colourCard colour (y : cs) noOfCards = tweedledumTweedledeeH (y : cs) noOfCards

singleNumberCard :: Number -> Integer -> Integer
singleNumberCard x noOfCards
    | even x     = noOfCards + 1
    | otherwise = noOfCards

numberCard :: Number -> [Either Colour Number] -> Integer -> Integer
numberCard number (y : cs) noOfCards
    | even number = tweedledumTweedledeeH (y : cs) (noOfCards + 1)
    | otherwise  = tweedledumTweedledeeH (y : cs) noOfCards
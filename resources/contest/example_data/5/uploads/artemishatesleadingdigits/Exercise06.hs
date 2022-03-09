module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen

type Number = Int

data Card = Card Colour Number

isGold :: Either Colour Number -> Bool
isGold (Left Gold) = True
isGold (Right n) = even n
isGold _ = False

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee cs = fromIntegral (length (filter isGold cs))

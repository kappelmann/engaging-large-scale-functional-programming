module Exercise06 where

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee = toInteger . length . filter shouldBeTurned

shouldBeTurned :: Either Colour Number -> Bool
shouldBeTurned (Left Gold) = True
shouldBeTurned (Right x)
    | even x = True
    | otherwise = False
shouldBeTurned _ = False
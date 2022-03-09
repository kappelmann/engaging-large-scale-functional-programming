module Exercise06 where

import Data.Either

data Colour = Gold | RustyBrown | TurtleGreen
    deriving (Eq, Ord, Show)

type Number = Int
data Card = Card Colour Number

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee ls
    | missingUnevens == 0 || goldenHearts == 0 || (missingUnevens > 0  && notGoldenHeart == 0) = 0
    | missingUnevens >= goldenHearts = toInteger goldenHearts
    | otherwise = toInteger goldenHearts
    where expectedUneven = let l = length ls in if even l then l `div` 2 else l `div` 2 + 1 
          actualUneven = length $ filter odd (map (fromRight 2) ls)
          notGoldenHeart = length $ filter (/= Gold) (map (fromLeft Gold) ls)
          goldenHearts = length $ filter (== Gold) (map (fromLeft RustyBrown) ls)
          missingUnevens = expectedUneven - actualUneven

-- test :: Either Colour Number
-- test = Left Gold

-- test2 :: Either Colour Number
-- test2 = Right 1

-- testList :: [Either Colour Number]
-- testList = [Left Gold, Right 1, Right 5, Right 2, Left TurtleGreen]


-- 40

-- 20 even
-- 20 uneven

-- 25 Numbers, 15 Colours -> 4 Gold, 6 RustyBrown, 5 TurtleGreen
-- 19 unev
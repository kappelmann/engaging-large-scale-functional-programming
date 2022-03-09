module Exercise06 where

import Data.Either

data Colour = Gold | RustyBrown | TurtleGreen
type Number = Int
data Card = Card Colour Number

-- tweedledumTweedledee :: [Either Colour Number] -> Integer
-- tweedledumTweedledee [] = 0
-- tweedledumTweedledee ((Right r):xs) = if even r then 1 + tweedledumTweedledee xs else 0 + tweedledumTweedledee xs
-- tweedledumTweedledee (_:xs) = 0

t :: [Either Colour Int]
t = [Left Gold, Right 1]

t2 :: [Card]
t2 = [Card Gold 2]

tweedledumTweedledee :: [Either Colour Number] -> Integer
tweedledumTweedledee [] = 0
tweedledumTweedledee ((Right r):xs) = if even r then 1 + tweedledumTweedledee xs else tweedledumTweedledee xs
tweedledumTweedledee ((Left Gold):xs) = 1 + tweedledumTweedledee xs
tweedledumTweedledee (_:xs) = tweedledumTweedledee xs

--list (xs:xss) = [[(eit, col, num)]]
--    where 
 --       eit = undefined
  --      col = [gold, rustyAndOld]
    --    num = [0...10^6]

--flip [] = []
--flip list (xs:xxs) =
 --   | xs == [(_,gold,)] = checkNum xs
  --  | otherwise = setCol col

--setCol col = rustyAndOld
  --      
--checkNum list xs
  --  | num == even = num+1 
  -- | otherwise = num


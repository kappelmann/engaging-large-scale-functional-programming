module Util where


import Data.List (nub, (\\))
import Solution


setPos :: (Pos, Field) -> Board -> Board
setPos (p, v) = updatePos (const v) 1 p

-- http://szudzik.com/ElegantPairing.pdf
pair :: (Ord a, Num a) => a -> a -> a
pair x' y'
  | x /= max x y  = y * y + x
  | otherwise     = x * x + x + y
  where
    x = x' - 2
    y = y' - 2
unpair :: Integral a => a -> (a, a)
unpair z
  | z - sq < root = (z - sq + 2, root + 2)
  | otherwise     = (root + 2, z - sq - root + 2)
  where
    root = floor $ sqrt $ fromIntegral z
    sq = root * root


size :: Board -> Size
size = Solution.size

makeBoard :: Size -> Board
makeBoard (h, w) = replicate h $ replicate w 0

criticalMass :: Size -> Pos -> Int
criticalMass s p = length $ Solution.neighbors s p

isValidSize :: Size -> Bool
isValidSize (y,x) = y >= 2 && x >= 2

checkWon :: Board -> Player
checkWon = single 0 . nub . map signum . filter (/=0) . concat

single :: a -> [a] -> a
single _ [x] = x
single x _   = x;

fields :: Board -> [(Pos, Field)]
fields = concatMap (\(r, fs) -> map (\(c, v) -> ((r, c), v)) fs) . zip [0 :: Int ..] . map (zip [0 :: Int ..])

newtype SetEq a = SetEq [a]
instance Show a => Show (SetEq a) where
  show (SetEq s) = show s
instance Eq a => Eq (SetEq a) where
  (SetEq xs) == (SetEq ys)
    | length xs /= length ys = False
    | otherwise              = null (xs \\ ys) && null (ys \\ xs)

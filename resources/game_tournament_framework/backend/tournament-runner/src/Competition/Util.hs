module Competition.Util where

import Competition.Types (Board, Field, Player, Pos, Size)
import Control.Monad (liftM2, forM)
import Data.List (unfoldr)
import Data.Array.IO
import System.Random (randomRIO)

-- create and new board of a given size
makeBoard :: Size -> Board
makeBoard (h, w) = replicate h $ replicate w 0

-- pretty print a board as a grid of numbers,
-- which represent the number of orbs in a cell,
-- players are are marked by ascii-escape sequences
-- player 1 is red, and player -1 is green
showBoard :: Board -> String
showBoard = unlines . map (unwords . map showField)

-- ascii escape sequences for coloring the board
escape :: Char
red :: String
green :: String
reset :: String
(escape, red, green, reset) = ('\27', escape : "[31m", escape : "[32m", escape : "[0m")

showField :: Field -> String
showField n
  | n > 0 = red ++ show n ++ reset
  | n < 0 = green ++ show (- n) ++ reset
  | otherwise = " "

{- VARIOUS UTILITY FUNCTIONS -}

-- perform action f on the the field v for player p
-- f modified the value of the field and the result will be set to be owned by p;
-- unless the new value is 0, then the cell will be unowned
updateValue :: (Int -> Int) -> Player -> Field -> Field
updateValue f p v = signum p * f (abs v)

-- updateValue with a different argument order
updateValue' :: Player -> Field -> (Int -> Int) -> Field
updateValue' p v f = updateValue f p v

-- create a list of all fields in a board with their positions
fields :: Board -> [(Pos, Field)]
fields = concatMap (\(r, fs) -> map (\(c, v) -> ((r, c), v)) fs) . zip intsFromZero . map (zip intsFromZero)

-- modify all fields of a board
mapBoard :: (Pos -> Field -> Field) -> Board -> Board
mapBoard f = zipWith (\r fs -> map (\(c, v) -> f (r, c) v) fs) intsFromZero . map (zip intsFromZero)

intsFromZero :: [Int]
intsFromZero = [0 ..]

width :: Board -> Int
width (x : _) = length x
width _ = 0

height :: Board -> Int
height = length

size :: Board -> Size
size = liftM2 (,) height width

-- calculate the critical mass (number of neighbours) of a cell for a board of a given size
-- must only be called for valid positions
criticalMass :: Size -> Pos -> Int
criticalMass = neighbours 0 $ const (1 +)

-- list all neighbours of a cell
-- must only be called for valid positions
neighboursList :: Size -> Pos -> [Pos]
neighboursList = neighbours [] (:)

-- fold over neighbours
-- must only be called for valid positions
neighbours :: a -> (Pos -> a -> a) -> Size -> Pos -> a
neighbours s op (h, w) (y, x) =
  go y 0 (-1) 0 $
    go x 0 0 (-1) $
      go y (h - 1) 1 0 $
        go x (w - 1) 0 1 s
  where
    go v c dy dx = if v /= c then op (dy + y, dx + x) else id

-- return the value of a single cell on the board
-- must only be called for valid positions
getField :: Pos -> Board -> Field
getField (y, x) b = b !! y !! x

-- update the value of a single cell
-- has no effect for invalid positions
updateField :: (Field -> Field) -> Pos -> Board -> Board
updateField u p = mapBoard (\pos v -> if pos == p then u v else v)

-- check if player p can put an orb on pos
-- must only be called for valid positions
validMove :: Player -> Pos -> Board -> Bool
validMove p pos b = signum (getField pos b) /= - p

playerToIndex :: Player -> Int
playerToIndex 1 = 0
playerToIndex (-1) = 1

-- https://stackoverflow.com/a/1780724
maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n
  where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

-- https://stackoverflow.com/a/12882583
chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
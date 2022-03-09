module Exercise05 where

import Data.List (intersect)
import qualified Data.Set as Set

type Pos = (Int, Int)

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [[showPos i j | j <- [0 .. w - 1]] | i <- [0 .. h - 1]]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag = help 0 [] h w rocks start flag

help :: Int -> [Pos] -> Int -> Int -> [Pos] -> Pos -> Pos -> Int
help count visited h w rocks current flag = if current `elem` visited then -1 else recursive
  where
    newpositions = map (direction h w current rocks flag) (possibleDir h w current)
    recursive = if flag `elem` newpositions then count else minimum $ map (\x -> help (count + 1) (x : visited) h w rocks x flag) newpositions

possibleDir :: Int -> Int -> Pos -> [Int]
possibleDir h w (a, b)
  | a == 0 && b == 0 = [1, 2]
  | a == 0 && b == w -1 = [2, 3]
  | a == h -1 && b == 0 = [0, 1]
  | a == h -1 && b == w -1 = [0, 3]
  | otherwise = [0, 1, 2, 3]

direction :: Int -> Int -> Pos -> [Pos] -> Pos -> Int -> Pos
direction h w current rocks flag dir
  | flag `elem` lines = flag
  | not $null inter = undefined
  | dir == 0 = (0, snd current)
  | dir == 1 = (fst current, w -1)
  | dir == 2 = (h -1, snd current)
  | dir == 3 = (fst current, 0)
  where
    inter = lines `intersect` rocks
    lines
      | dir == 0 = [(0, x) | x <- [0 .. snd current]]
      | dir == 1 = [(x, w -1) | x <- [fst current .. h -1]]
      | dir == 2 = [(h -1, x) | x <- [snd current .. w -1]]
      | dir == 3 = [(x, 0) | x <- [0 .. fst current]]

main :: IO ()
main = print $ incredibleGame 5 5 [(3, 3), (4, 2)] (0, 4) (1, 3)
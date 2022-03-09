module Exercise05 where

import Prelude hiding (Left, Right)
import Data.Function
import qualified Data.List as List
import qualified Data.Set as Set


data Direction = Left | Right | Up | Down
    deriving (Eq, Show)

type Pos = (Int,Int)

type Maze = Set.Set Pos

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

neighbors :: Int -> Int -> Pos -> [Pos] -> Pos -> [Pos]
neighbors h w pos@(r,c) rocks flag
  | flag == next_left || flag == next_right || flag == next_up || flag == next_down = [flag]
  | otherwise = [
        (\(r',c') -> (r',c'+1)) next_left,
        (\(r',c') -> (r',c'-1)) next_right,
        (\(r',c') -> (r'+1,c')) next_up,
        (\(r',c') -> (r'-1,c')) next_down
    ]
    where
        same_r = filter (\(r',_) -> r' == r) rocks
        same_c = filter (\(_,c') -> c' == c) rocks
        (same_r_left, same_r_right) = List.partition (\(_,c') -> c' < c) same_r
        (same_c_up, same_c_down) = List.partition (\(r',_) -> r' < r) same_c
        next_left = List.maximumBy (compare `on` snd) ((r,-1):same_r_left)
        next_right = List.minimumBy (compare `on` snd) ((r,w):same_r_right)
        next_up = List.maximumBy (compare `on` fst) ((-1,c):same_c_up)
        next_down = List.minimumBy (compare `on` fst) ((h,c):same_c_down)

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks pos@(r,c) flag@(f_r, f_c) = search h w rocks [pos] flag [] 1


search :: Int -> Int -> [Pos] -> [Pos] -> Pos -> [Pos] -> Int -> Int
search h w rocks poss flag@(f_r, f_c) visiteds depth
  | List.null possiblePoss = -1
  | flag `elem` possiblePoss = depth
  | otherwise = search h w rocks possiblePoss flag (poss ++ visiteds) (depth + 1)
    where possiblePoss = List.nub $ filter (`notElem` visiteds) $ concatMap (\pos -> neighbors h w pos (flag:rocks) flag) poss

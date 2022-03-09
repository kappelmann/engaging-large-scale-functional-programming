module Exercise05 where

import qualified Data.Set as Set
import Data.List (nub)

type Pos = (Int,Int)

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag
  | start == flag = 0
  | otherwise     = incredibleGame' h w (Set.fromList rocks) Set.empty [start] flag 1

incredibleGame' :: Int -> Int -> Set.Set Pos -> Set.Set Pos -> [Pos] -> Pos -> Int -> Int
incredibleGame' h w rocks posOld posNew flag nMoves
  | found         = nMoves
  | null posNew'  = -1
  | otherwise     = incredibleGame' h w rocks posOld' posNew' flag (nMoves + 1)
  where
    (posNew'', found) = newPoss h w rocks posNew flag
    posNew' = filter (`Set.notMember` posOld) posNew''
    posOld' = insertAll posOld posNew

insertAll :: Set.Set Pos -> [Pos] -> Set.Set Pos
insertAll = foldl (flip Set.insert)

newPoss :: Int -> Int -> Set.Set Pos -> [Pos] -> Pos -> ([Pos], Bool)
newPoss h w rocks poss flag = (justPoss, found)
  where
    newP = concat $ [map (\p -> newPos h w rocks p flag dir) poss | dir <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]
    justPoss = nub $ map fst newP
    found = any snd newP

newPos :: Int -> Int -> Set.Set Pos -> Pos -> Pos -> (Int, Int) -> (Pos, Bool)
newPos h w rocks (y, x) flag dir@(yDir, xDir)
  | xNew < 0 || xNew >= w || yNew < 0 || yNew >= h  = ((y, x), False)
  | (yNew, xNew) `Set.member` rocks = ((y, x), (y, x) == flag)
  | otherwise = if (yNew, xNew) == flag then ((yNew, xNew), True) else newPos h w rocks (yNew, xNew) flag dir
  where (yNew, xNew) = (y + yDir, x + xDir)

module Exercise05 where

import Data.Set (Set, member, insert, empty)
import qualified Data.Set
import Data.List (filter, null, nub)
--import Debug.Trace
type Pos = (Int,Int)


data Dir = U | R | D | L deriving (Eq, Show, Ord, Read)

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag = search h w rocks start flag 0 Data.Set.empty

search :: Int -> Int -> [Pos] -> Pos -> Pos -> Int -> Set Pos -> Int
search h w rocks pos flag len exp
 -- | trace ("search " ++ show pos ++ " " ++ show len ++ " " ++ show exp) False = undefined
  | pos == flag = len
  | pos `member` exp = -1
  | otherwise = if null filtered then -1 else minimum filtered
    where filtered = filter (>0) [search h w rocks newPos flag (len+1) (pos `insert` exp) | newPos <- getNextMoves' h w rocks flag pos]

getNextMoves :: Int -> Int -> [Pos] -> Pos -> Pos -> [Pos]
getNextMoves h w rocks flag pos@(y,x) 
  | (x == 0) || any (\(ry, rx) -> rx == x-1) rocks = map (slide h w rocks pos flag) $ if ( y == 0 || any (\(ry, rx) -> ry == y-1) rocks) then [R,D] else [U,R,D] --nicht links
  | (y == 0) || any (\(ry, rx) -> ry == y-1) rocks = map (slide h w rocks pos flag) $ if ( x == w-1 || any (\(ry, rx) -> rx == x+1) rocks) then [D,L] else [R,D,L] --nicht hoch
  | (x == w-1) || any (\(ry, rx) -> rx == x+1) rocks = map (slide h w rocks pos flag) $ if (y == h-1 || any (\(ry, rx) -> ry == y+1) rocks) then [U,L] else [U,D,L] --nicht rechts
  | (y == h-1) || any (\(ry, rx) -> ry == y+1) rocks = map (slide h w rocks pos flag) $ if (x == 0 || any (\(ry, rx) -> rx == x-1) rocks) then [U,R] else [U,R,L] --nicht runter
  | otherwise = map (slide h w rocks pos flag) [U,R,D,L]

getNextMoves' :: Int -> Int -> [Pos] -> Pos -> Pos -> [Pos]
getNextMoves' h w rocks flag pos = nub $ filter (/=pos) $ map (slide h w rocks pos flag) [U, R, D, L]

slide1 :: Int -> Int -> [Pos] -> Pos -> Pos -> Dir -> Pos
slide1 h w rocks (y,x) (fy, fx) U = let newY = maximum (0 :  [ry + 1 | (ry,rx) <- rocks, rx == x]) in (newY, x)
slide1 h w rocks (y,x) (fy, fx) L = let newX = maximum (0 : [rx + 1 | (ry,rx) <- rocks, ry == y]) in (y, newX)
slide1 h w rocks (y,x) (fy, fx) D = let newY = minimum ( (h-1) : [ry + 1 | (ry,rx) <- rocks, rx == x]) in (newY, x)
slide1 h w rocks (y,x) (fy, fx) R = let newX = minimum ( (w-1) : [rx + 1 | (ry,rx) <- rocks, ry == y]) in (y, newX)

slide :: Int -> Int -> [Pos] -> Pos -> Pos -> Dir -> Pos
slide h w rocks (y,x) f@(fy,fx) U 
    | fx == x = until (\pos@(y',x') -> pos == f ||y' == 0 || (y'-1,x' ) `elem` rocks) (\(y', x') -> (y'-1, x')) (y,x)
    | otherwise = let newY = maximum (0 :  [ry + 1 | (ry,rx) <- rocks, rx == x]) in (newY, x)
slide h w rocks (y,x) f@(fy, fx) L 
    | fy == y = until (\pos@(y',x') -> pos == f ||x' == 0 || (y',x'-1 ) `elem` rocks) (\(y', x') -> (y', x'-1)) (y,x)
    | otherwise = let newX = maximum (0 : [rx + 1 | (ry,rx) <- rocks, ry == y]) in (y, newX)
slide h w rocks (y,x) f@(fy, fx) D 
    | fx == x = until (\pos@(y',x') -> pos == f ||y' == h-1 || (y'+1,x' ) `elem` rocks) (\(y', x') -> (y'+1, x')) (y,x)
    | otherwise = let newY = minimum ( (h-1) : [ry + 1 | (ry,rx) <- rocks, rx == x]) in (newY, x)
slide h w rocks (y,x) f@(fy, fx) R 
    | fy == y = until (\pos@(y',x') -> pos == f ||x' == w-1 || (y',x'+1 ) `elem` rocks) (\(y', x') -> (y', x'+1)) (y,x)
    | otherwise = let newX = minimum ( (w-1) : [rx + 1 | (ry,rx) <- rocks, ry == y]) in (y, newX)

-- slide 10 10 [(7,2),(3,3),(4,9)] (4,3) (4,7)
--incredibleGame 10 10 [(7,2),(3,3),(4,9)] (7,6) (4,7)
-- 
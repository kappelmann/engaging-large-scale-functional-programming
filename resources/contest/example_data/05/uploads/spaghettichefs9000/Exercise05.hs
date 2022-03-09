module Exercise05 where

import Data.Array
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int,Int)

type Maze = Array Pos Int
flag :: Int
flag = -1

rock :: Int
rock = -2

ice :: Int
ice = 0

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start f = let m = createMaze h w rocks f
                                   in bfSearch' 1 [start] m

createMaze :: Int -> Int -> [Pos] -> Pos -> Maze
createMaze h w rocks f = let e = listArray ((0,0), (h-1, w-1)) (repeat ice) :: Maze
                         in e // ((f, flag):map (fillWith rock) rocks)

fillWith :: Int -> Pos -> (Pos, Int)
fillWith n p = (p, n)

bfSearch :: Int -> [Pos] -> Maze -> Int
bfSearch n ps m = let n'    = concatMap (neighbours m) ps
                      new   = nub n'
                      found = any (isFlag m) n'
                      m'    = m // map (fillWith n) n'
                  in if null n' then -1 else if found then n else bfSearch (n + 1) new m'

bfSearch' :: Int -> [Pos] -> Maze -> Int
bfSearch' n ps m = case allNeighbours m ps [] of
                     Nothing -> n
                     Just ns -> if null ns then -1 else bfSearch' (n + 1) (nub ns) (m // map (fillWith n) ns)

allNeighbours :: Maze -> [Pos] -> [Pos]-> Maybe [Pos]
allNeighbours _ [] ns     = Just ns
allNeighbours m (x:xs) ns = let n' = neighbours m x
                                found = any (isFlag m) n'
                            in if found then Nothing else allNeighbours m xs (n' ++ ns)

isFlag :: Maze -> Pos -> Bool
isFlag m p = (m ! p) == flag

isRock :: Maze -> Pos -> Bool
isRock m p = (m ! p) == rock

neighbours :: Maze -> Pos -> [Pos]
neighbours m p = filter (valid m) [slideNegX m p, slideNegY m p, slidePosX m p, slidePosY m p]

slideNegX :: Maze -> Pos -> Pos
slideNegX m p@(y, x) = let ((_, x0), _) = bounds m
                           next = (y, x-1)
                       in if x == x0 || isRock m next || isFlag m p then p else slideNegX m next

slidePosX :: Maze -> Pos -> Pos
slidePosX m p@(y, x) = let (_, (_, x1)) = bounds m
                           next = (y, x+1)
                       in if x == x1 || isRock m next || isFlag m p then p else slidePosX m next

slidePosY :: Maze -> Pos -> Pos
slidePosY m p@(y, x) = let (_, (y1, _)) = bounds m
                           next = (y+1, x)
                       in if y == y1 || isRock m next || isFlag m p then p else slidePosY m next

slideNegY :: Maze -> Pos -> Pos
slideNegY m p@(y, x) = let ((y0, _), _) = bounds m
                           next = (y-1, x)
                       in if y == y0 || isRock m next || isFlag m p then p else slideNegY m next

valid :: Maze -> Pos -> Bool
valid m p = ((m ! p) == flag) || ((m ! p) == ice)

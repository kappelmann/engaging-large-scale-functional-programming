module Exercise05 where
import Data.List
-- import Debug.Trace
import Data.Ord
import qualified Data.Set as Set


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
incredibleGame h w rocks start flag = fst (dfs (Set.fromList []) start)
  where
    -- traceDFS v pos = trace (show pos) (dfs v pos)
    dfs v (i,j)
      | not (i >= 0 && i < h && j >= 0 && j < w) = (-1, Set.insert (i,j) v)
      | Set.member (i,j) v = (-1, Set.insert (i,j) v)
      | (i,j) == flag = (0, Set.insert (i,j) v)
      | crossedVer || crossedHor = (1, Set.insert (i,j) v)
      | not (Prelude.null results) = (1 + minimum results, Set.insert (i,j) v)
      | otherwise = (-1, Set.insert (i,j) v)
      where
        results = Prelude.filter (>=0) [fst dr, fst dl, fst dd, fst du]

        dr = dfs (Set.insert (i,j) v) sr
        dl = dfs (Set.insert (i,j) (snd dr)) sl
        dd = dfs (Set.insert (i,j) (snd dl)) sd
        du = dfs (Set.insert (i,j) (snd dd)) su

        crossedVer = j == snd flag && (i < fst flag && fst sd > fst flag || i > fst flag && fst su < fst flag)
        crossedHor = i == fst flag && (j < snd flag && snd sr > snd flag || j > snd flag && snd sl < snd flag)
        sr
          | not (Prelude.null rs) = minimumBy (comparing snd) rs
          | otherwise = (i, w-1)
          where
            rs = [(x,y-1) | (x,y) <- rocks, x == i, y > j]

        sl
          | not (Prelude.null rs) = maximumBy (comparing snd) rs
          | otherwise = (i, 0)
            where 
               rs = [(x,y+1) | (x,y) <- rocks, x == i, y < j]

        sd
          | not (Prelude.null rs) = minimumBy (comparing fst) rs
          | otherwise = (h-1, j)
          where 
            rs = [(x-1,y) | (x,y) <- rocks, x > i, y == j]

        su
          | not (Prelude.null rs) = maximumBy (comparing fst) rs
          | otherwise = (0, j)
            where 
              rs = [(x+1,y) | (x,y) <- rocks, x < i, y == j]

test1 :: Int
test1 = incredibleGame 5 5 [(3,3),(4,2)] (0,4) (1,3)

test2 :: Int
test2 = incredibleGame 10 10 [(7,2),(3,3),(4,9)] (7,6) (4,7)

-- w=h=5, rocks=[(3,3),(4,2)], start=(0,4), flag=(1,3),
-- w=h=10, rocks=[(7,2),(3,3),(4,9)], start=(7,6), flag=(4,7)

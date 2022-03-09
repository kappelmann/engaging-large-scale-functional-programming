module Exercise05 where

-- import System.Random

import Data.Array.Unboxed
import qualified Data.Set as S

-- import Debug.Trace (traceShow, trace)

type Pos = (Int,Int)
data Dir = N | S | E | W

rock = 'r'
visited = 'v'
notSignificant = 'n'

test = putStrLn (showMaze 5 5 [(1,2), (2,4), (2,2)] (0,0) (4,4))

{-
test2 = do
  g <- getStdGen
  let lim = 1000
  let rs = fmap conv $ (randomRs (0, lim) g)
      conv [] = []
      conv (a:b:rs) = (a,b) : conv rs
      (rocks, flag:start:xs) = split 200 rs

  let res = incredibleGame lim lim rs start flag
  print res
-}

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

type Pos' = (Int, Pos)

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag = if flag `elem` p0 then 1 else go 0 g0' S.empty
    where
      fld = genField h w rocks
      gg = mgen h w fld
      (g0, p0) = gg start
      g0' = fmap (\x -> (2, x)) g0
      go :: Int -> [Pos'] -> S.Set Pos -> Int
      go 1000 _ _ = -1 -- error "timeout"
      go fuel ((iter,u):us) ps =
          if flag `elem` passing
            then iter
            -- trace (show iter ++ ", " ++ show  new') $
            else go (fuel + 1) (us ++ new') (S.insert u ps)
        where
          (new, passing) = gg u
          new' = fmap (\x -> (iter + 1, x)) . filter (\x -> not $ S.member x ps) $ new
      go _ _ _ = -1
      -- (_, num) = incredibleGame' ( repeat w (repeat h 0)) rocks

type Field = UArray Pos Bool
genField :: Int -> Int -> [Pos] -> Field
genField h w rocks = array ((0,0), (h-1, w-1)) assocs
  where
    assocs0 =  [ ((y, x), (y,x) `elem` rocks)  | y <- [0..h-1], x <- [0..w-1] ]
    assocs = [ (r, True) | r <- rocks]


--trace ("mgen at " ++ show currpos ++ show up ++ show down ++ show left ++ show right)  $
mgen :: Int -> Int -> Field -> Pos -> ([Pos], [Pos])
mgen h w fld currpos@(cy, cx) =  (final, passing)
  where
    res = [ up , down, left, right ]
    passing = concat res
    final = concatMap last' res
    last' xs = if null xs then [] else [last xs]
    up    = let cy' = (cy-1) in if cy  == 0 then [] else takeWhile (\idx -> not (fld ! idx)) . reverse . fmap (\y -> (y, cx)) $ [ 0  ..cy'   ]
    down  = let cy' = (cy+1) in if cy' == h then [] else takeWhile (\idx -> not (fld ! idx)) .           fmap (\y -> (y, cx)) $ [ cy'..(h-1) ]
    left  = let cx' = (cx-1) in if cx  == 0 then [] else takeWhile (\idx -> not (fld ! idx)) . reverse . fmap (\x -> (cy, x)) $ [ 0  ..cx'   ]
    right = let cx' = (cx+1) in if cx' == w then [] else takeWhile (\idx -> not (fld ! idx)) .           fmap (\x -> (cy, x)) $ [ cx'..(w-1) ]


-- getNextBestTurn :: [[Char]] -> Pos -> Pos
-- getNextBestTurn field from to = undefined

{-
isWayValid :: [[Char]] -> Pos -> Dir -> Bool
isWayValid field (x,y) dir
  | dir == N =
      let slope = field !! x
          line = reverse (take y slope)
          dong = takeWhile (/= 'r') line
          l = last dong
      in
        l /= 'v'
  | dir == S = let slope = field !! x
                   line = drop (y + 1) slope
                   dong = takeWhile (/= 'r') line
                   l = last dong in
                      l /= 'v'
  | dir == E = let  turnAround = transpose field
                    slope = turnAround !! y
                    line = drop (y + 1) slope
                    dong = takeWhile (/= 'r') line
                    l = last dong in
                      l /= 'v'

validWays :: Int -> Int -> [Pos] -> [Dir]
validWays h w rocks = undefined

nextRockAtDir :: Dir -> Pos
nextRockAtDir dir = undefined


-}
-- bestWay :: [Dir] -> Pos -> Pos -> [Dir]
-- bestWay [] _ _ = []
-- bestWay (x:dirs) (fx, fy) (tx,ty)
--   | xd > yd && fx < tx =

--   where
--     xd = abs (fx - tx)
--     yd = abs (fy - ty)


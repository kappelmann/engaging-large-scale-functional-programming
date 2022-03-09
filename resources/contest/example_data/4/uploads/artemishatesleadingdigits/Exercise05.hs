{-# LANGUAGE TupleSections #-}

module Exercise05 where

import Data.Array (Array, array, bounds, (!), (//))

type Pos = (Int, Int)

data E = Blank | Rock | Me | Flag | Old | Pass
  deriving (Eq)

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [[showPos i j | j <- [0 .. w - 1]] | i <- [0 .. h - 1]]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

isValid :: Array (Int, Int) E -> (Int, Int) -> Bool
isValid game (x, y) = here /= Rock
  where
    here = game ! (x, y)

isGood :: Array (Int, Int) E -> (Int, Int) -> Bool
isGood game (x, y) = here /= Old && here /= Pass
  where
    here = game ! (x, y)

isNice :: Array (Int, Int) E -> (Int, Int) -> Bool
isNice game (x, y) = here /= Old && here /= Me
  where
    here = game ! (x, y)

fill :: Array (Int, Int) E -> [(Int, Int)] -> Array (Int, Int) E
fill game fs = game // map (,Pass) (filter (isGood game) fs)

propG :: (Int, Int) -> Array (Int, Int) E -> ([(Int, Int)], Array (Int, Int) E)
propG (x, y) game = (goodNewHeads, filledMapAgainA)
  where
    (_, (maxX, maxY)) = bounds game
    up = reverse (takeWhile (isValid game) (map (,y) [x .. 0]))
    (upFirst : ups) = up
    down = reverse (takeWhile (isValid game) (map (,y) [x .. maxX]))
    (downFirst : downs) = down
    left = reverse (takeWhile (isValid game) (map (x,) [y .. 0]))
    (leftFirst : lefts) = left
    right = reverse (takeWhile (isValid game) (map (x,) [y .. maxY]))
    (rightFirst : rights) = right
    toFills = (if (not . null) up then ups else []) ++ (if (not . null) down then downs else []) ++ (if (not . null) left then lefts else []) ++ (if (not . null) right then rights else [])
    filledMap = fill game toFills
    newHeads = [upFirst | (not . null) up] ++ [downFirst | (not . null) down] ++ [leftFirst | (not . null) left] ++ [rightFirst | (not . null) right]
    goodNewHeads = filter (isNice filledMap) newHeads
    filledMapAgain = filledMap // map (,Me) goodNewHeads
    filledMapAgainA = filledMapAgain // [((x, y), Old)]

step :: [(Int, Int)] -> Array (Int, Int) E -> ([(Int, Int)], Array (Int, Int) E)
step [] m = ([], m)
step (head : hs) m = (restH ++ now, nowMap)
  where
    (restH, afterMap) = step hs m
    (now, nowMap) = propG head afterMap

walk :: [(Int, Int)] -> (Int, Int) -> Array (Int, Int) E -> Int
walk [] flag map = -1
walk hs flag map = if afterMap ! flag == Flag then 1 + walk hs flag afterMap else 1
  where
    (hs, afterMap) = step hs map

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag
  | start == flag = 0
  | otherwise = walk [start] flag fullMap
  where
    blankMap = array ((0, 0), (h, w)) [((x, y), Blank) | x <- [0 .. h -1], y <- [0 .. w -1]]
    fullMap = blankMap // ((start, Me) : (flag, Flag) : map (,Rock) rocks)

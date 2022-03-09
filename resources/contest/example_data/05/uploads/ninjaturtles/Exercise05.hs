module Exercise05 where

import Data.List as L
import Data.Map as M
import Data.Maybe

type Pos = (Int, Int)

data Move = L | R | U | D

data Obs = Rock | F | E

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [[showPos i j | j <- [0 .. w - 1]] | i <- [0 .. h - 1]]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag = snd $ inc h w (board rocks start flag) start flag 0

inc :: Int -> Int -> Map Pos Char -> Pos -> Pos -> Int -> (Map Pos Char, Int)
inc h w m pos flag n
  | pos == flag = (mp, n)
  | L.null ss = (fst ls, -1)
  | otherwise = (fst ls, minimum ss)
  where
    ss = L.filter (/= -1) (snd ls)
    ls = L.mapAccumL (\ma p -> inc h w ma p flag (n + 1)) mp goals
    goals = L.map (fromMaybe (-1, -1)) (L.filter (\p -> isJust p && M.lookup (fromMaybe (-1, -1) p) mp /= Just 'S') (L.map (\dir -> checkGoal h w pos flag dir mp) [L, R, U, D]))
    mp = M.insert pos 'S' m

checkGoal :: Int -> Int -> Pos -> Pos -> Move -> Map Pos Char -> Maybe Pos
checkGoal h w p flag mov m = if flag == p then Just p else getGoal h w p flag mov m

getGoal :: Int -> Int -> Pos -> Pos -> Move -> Map Pos Char -> Maybe Pos
getGoal _ _ (x, 0) flag L m = Just (x, 0)
getGoal h w (x, y) flag L m = if isEmpty (x, y -1) m then checkGoal h w (x, y -1) flag L m else Just (x, y)
getGoal h w p@(x, y) flag R m
  | w -1 == y = Just (x, y)
  | isEmpty (x, y + 1) m = checkGoal h w (x, y + 1) flag R m
  | otherwise = Just (x, y)
getGoal h w p@(x, y) flag U m
  | x == 0 = Just (x, y)
  | isEmpty (x -1, y) m = checkGoal h w (x -1, y) flag U m
  | otherwise = Just (x, y)
getGoal h w p@(x, y) flag D m
  | x == h -1 = Just (x, y)
  | isEmpty (x + 1, y) m = checkGoal h w (x + 1, y) flag D m
  | otherwise = Just (x, y)

isEmpty :: Pos -> Map Pos Char -> Bool
isEmpty p m = M.lookup p m /= Just 'R'

step :: Pos -> Move -> Pos
step (x, y) L = (x, y -1)
step (x, y) R = (x, y + 1)
step (x, y) U = (x -1, y)
step (x, y) D = (x + 1, y)

getMoves :: Pos -> Map Pos Char -> [Pos]
getMoves (x, y) board = L.filter (\pos -> M.lookup pos board == Just 'E') [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

board :: [Pos] -> Pos -> Pos -> Map Pos Char
board r s f = L.foldl (\m x -> M.insert x 'R' m) (M.insert s 'S' empty) r

test :: Int
test = incredibleGame 5 5 [(3, 3), (4, 2)] (0, 4) (1, 3)

test2 :: Int
test2 = incredibleGame 10 10 [(7, 2), (3, 3), (4, 8)] (7, 6) (4, 7)

test3 :: Int
test3 =
  incredibleGame
    99
    99
    [(1, 95), (3, 29), (4, 66), (4, 97), (4, 99), (6, 6), (7, 73), (8, 6), (8, 28), (8, 50), (9, 4), (9, 38), (9, 41), (9, 50), (11, 0), (11, 45), (12, 47), (15, 78), (16, 60), (17, 38), (18, 46), (19, 78), (19, 97), (22, 49), (25, 59), (26, 10), (26, 14), (27, 46), (27, 65), (27, 79), (28, 52), (29, 56), (29, 92), (30, 2), (30, 90), (31, 32), (31, 90), (34, 65), (35, 4), (35, 10), (38, 35), (39, 76), (40, 1), (40, 4), (40, 40), (43, 53), (45, 85), (48, 56), (48, 59), (50, 56), (51, 11), (52, 72), (52, 95), (54, 2), (55, 1), (55, 90), (57, 75), (58, 27), (59, 75), (60, 83), (61, 17), (61, 40), (62, 11), (64, 49), (64, 85), (64, 99), (65, 25), (67, 72), (68, 49), (68, 85), (69, 19), (69, 54), (69, 89), (70, 12), (72, 38), (73, 20), (74, 70), (76, 18), (76, 19), (76, 30), (76, 74), (76, 90), (77, 72), (78, 86), (79, 44), (81, 27), (82, 52), (83, 56), (83, 65), (86, 1), (86, 34), (86, 79), (87, 85), (89, 58), (90, 21), (91, 45), (91, 62), (92, 27), (93, 88), (94, 70), (96, 33)]
    (0, 0)
    (1, 1)

goalTest :: [Pos]
goalTest = L.filter (\p -> M.lookup p (M.insert (7, 6) 'S' (board [(7, 5), (3, 3), (4, 8)] (7, 6) (4, 7))) /= Just 'S') (L.map (\m -> fromMaybe (-1, -1) (checkGoal 10 10 (7, 6) (4, 7) m (board [(7, 5), (3, 3), (4, 8)] (7, 6) (4, 7)))) [L, R, U, D])
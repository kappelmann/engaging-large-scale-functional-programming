module Exercise05 where

type Pos = (Int,Int)

data Dir = U | D | L | R

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag = undefined

-- goAllDirections :: Int -> Int -> [Pos] -> Pos -> Pos -> Int -> [Pos] -> Int
-- goAllDirections h w rocks current flag count visited
--   | current == (h+1,w+1) = -1
--   | current == flag = count
--   | otherwise = [gU, gD, gR, gL]
--     where
--       gU = goAllDirections h w rocks (goUp h w rocks current flag current:visited)
--       gD = goAllDirections (goDown h w rocks current flag current:visited)
--       gL = goAllDirections (goLeft h w rocks current flag current:visited)
--       gR = goAllDirections (goRight h w rocks current flag current:visited)

goUp :: Int -> Int -> [Pos] -> Pos -> Pos -> [Pos] -> Pos
goUp h w rocks current flag visited = if check `elem` visited || fst current > h then (h+1, w+1) else check
  where column = filter (\x -> snd x == snd current) rocks
        check = if null column then (0,snd current) else (fst (last column)+1,snd current)

goDown :: Int -> Int -> [Pos] -> Pos -> Pos -> [Pos] -> Pos
goDown h w rocks current flag visited = if check `elem` visited || fst current > h then (h+1, w+1) else check
  where column = filter (\x -> snd x == snd current) rocks
        check = if null column then (h,snd current) else (fst (head column)-1,snd current)

goRight :: Int -> Int -> [Pos] -> Pos -> Pos -> [Pos] -> Pos
goRight h w rocks current flag visited = if check `elem` visited || fst current > h then (h+1, w+1) else check
  where row = filter (\x -> fst x == fst current) rocks
        check = if null row then (fst current,w) else (fst current,snd (head row)-1)

goLeft :: Int -> Int -> [Pos] -> Pos -> Pos -> [Pos] -> Pos
goLeft h w rocks current flag visited = if check `elem` visited || fst current > h then (h+1, w+1) else check
  where row = filter (\x -> fst x == fst current) rocks
        check = if null row then (fst current,w) else (fst current,snd (last row)+1)


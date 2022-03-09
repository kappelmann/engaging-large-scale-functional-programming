module Exercise05 where
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
incredibleGame h w rocks start flag = findThePath start flag [start]
  where paths list = if null list then (-1) else minimum list 
        findThePath stop toReach wasThere 
                |fst stop == fst toReach = paths [b|b <- [goThere stop 2 toReach wasThere, goThere stop 0 toReach wasThere], b > (-1)]
                |snd stop == snd toReach = paths [b|b <- [goThere stop 1 toReach wasThere, goThere stop 3 toReach wasThere], b > (-1)]
                |otherwise = paths [b|b <- [goThere stop 2 toReach wasThere, goThere stop 0 toReach wasThere, goThere stop 1 toReach wasThere, goThere stop 3 toReach wasThere], b > (-1)]
        goThere pos dir toReach wasThere
          |pos == toReach = 0

          |dir == 0 =  if (snd pos <= 0) || (fst pos, snd pos - 1) `elem` rocks then (-1) 
                       else let stop =  head [(fst pos, y)| y <- reverse [0..(snd pos-1)], (fst pos, y-1) `elem` rocks || y == 0] in
                       if (fst pos == fst toReach) && (snd toReach >= snd stop ) && (snd toReach <= snd pos) then 1
                       else let antwort = findThePath stop toReach (stop:wasThere) in if stop `elem` wasThere||antwort == (-1) then (-1) else 1 + antwort
                        

          |dir == 1 = if (fst pos <= 0) || (fst pos-1, snd pos) `elem` rocks then (-1) 
                      else let stop =  head [(x, snd pos)| x <- reverse [0..(fst pos-1)], (x-1, snd pos) `elem` rocks || x == 0] in
                      if (snd pos == snd toReach) && (fst toReach <= fst pos ) && (fst toReach >= fst stop) then 1
                      else let antwort = findThePath stop toReach (stop:wasThere) in  if stop `elem` wasThere||antwort == (-1) then (-1) else 1 + antwort
                     

          |dir == 2 =  if (snd pos >= h-1) || (fst pos, snd pos + 1) `elem` rocks then (-1) 
                       else let stop =  head [(fst pos, y)| y <- [(snd pos)..h-1], (fst pos, y+1) `elem` rocks || y == h-1] in
                       if (fst pos == fst toReach) && (snd toReach >= snd pos ) && (snd toReach <= snd stop) then 1
                       else let antwort = findThePath stop toReach (stop:wasThere) in if stop `elem` wasThere||antwort == (-1) then (-1) else 1 + antwort


          |dir == 3 = if (fst pos >= w-1) || (fst pos+1, snd pos) `elem` rocks then (-1) 
                      else let stop =  head (filter (\(x,y)->(x+1,y) `elem` rocks|| x == w-1) [(x, snd pos)| x <- [(fst pos)..w-1]]) in
                      if (snd pos == snd toReach) && (fst toReach >= fst pos ) && (fst toReach <= fst stop) then 1
                      else let antwort = findThePath stop toReach (stop:wasThere) in if stop `elem` wasThere||antwort == (-1) then (-1) else 1 + antwort
                      



module Exercise04 where

--rudolph :: String -> (Int, Int)
--rudolph string = recRudolph string 0 0


rudolph string = if null res || m == 0 then (0,1) else (m, count res m)
    where
        list = map mapper string
        res = step list
        m = maximum res


count [] _ = 0
count (x:xs) c = if c==x then 1 + count xs c else count xs c


step [] = []
step xs = if l == 0 then step (tail xs) else l : step (drop l xs)
    where
        l = getFrom xs 0 0

getFrom [] c d = if c /= 0 then 0 else d
getFrom (x:xs) c d 
    |c > 0 = getFrom xs (c+x) (d+1)
    |c == 0 = max d (getFrom xs (c+x) (d+1))
    |otherwise = 0

mapper :: Char -> Int 
mapper 'L' = -1
mapper _ = 1


recRudolph [] maxLength count = if maxLength == 0 then (0,1) else (maxLength, count)
recRudolph string@(s:ss) maxLength count
    |newLength > maxLength  = if newLength > length ss then (newLength, 1) else recRudolph ss newLength 1
    |newLength == maxLength = if maxLength > length ss then (maxLength, count+1) else recRudolph ss maxLength (count+1)
    |otherwise              = if maxLength > length ss then (maxLength, count) else recRudolph ss maxLength count
        where newLength = lengthFrom string 0 0 0

lengthFrom [] path n lastCorrect = if path == 0 then n else lastCorrect
lengthFrom (s:ss) path n lastCorrect
    |s == 'L'   = if path == 0 then n else lengthFrom ss (path - 1) (n+1) newCorrect
    |otherwise  = lengthFrom ss (path+1) (n+1) newCorrect
        where newCorrect = if path == 0 then n else lastCorrect


--mapper :: Char -> Int 
--mapper 'L' = -1
--mapper _ = 1

--count [] _ = 0
--count (x:xs) c = if c==x then 1 + count xs c else count xs c
--
--greedyGreedy [] _ _ = []
--greedyGreedy (d:ds) c l
--    | n > 0 = greedyGreedy ds n (l+1)
--    | n == 0 = l : greedyGreedy ds 0 (l+1)
--    | otherwise = greedyGreedy ds 0 0
--    where
--        v = if d == 'L' then -1 else 1
--        n = c + v
--
--mapper :: Char -> Int 
--mapper 'L' = -1
--mapper _ = 1
----
----mapToHeigths [] c = []
----mapToHeigths ('L':ds) c = c-1 : mapToHeigths ds (c-1)
----mapToHeigths ('R':ds) c = c+1 : mapToHeigths ds (c+1)
----
--
--ascend (x:xs) c = if x>0 then ascend xs (c+1) else --descend (x:xs) c
--
----moveRight :: [Int] -> [Int] -> Int -> Int -> [Int]
----moveRight [] [] c d = if c==0 then [d] else []
----moveRight (l:ls) [] c d  = let (dif, l') = moveLeft (l:ls) c 0 in if c == 0 then [d] else if checkP l' 0 then [d-dif| dif /= -1] else moveRight l' l' 0 0
----moveRight l (r:rs) c d
----    | c  > 0 = moveRight l rs (c+r) (d+1)
----    | c == 0 = d:moveRight l rs r (d+1)
----    | otherwise = let (dif, ls) = moveLeft l c 0 in if dif == -1 then [] else moveRight ls (r:rs) 0 (d-dif)
----
----moveLeft [] _ _ = (-1, [])
------moveLeft (x:xs) c d = if c-x == 0 then (d + 1, xs) else moveLeft xs (c-x) (d+1)
----moveLeft (x:xs) c d = if c == 0 then (d, x:xs) else moveLeft xs (c-x) (d+1) 
----
----checkP [] _ = True 
----checkP (x:xs) c = if x+c<0 then False else checkP xs (c+x)
----
---- test s = rudolph s == rudolph2 s
--
--rak string = moveRight list list 0 0
--    where
--        list = map mapper string
--    
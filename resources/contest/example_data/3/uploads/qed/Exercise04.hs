module Exercise04 where

rudolph :: String -> (Int, Int) 
rudolph str = let sSize = subStrings str
                  maxSize = if null sSize then 0 else maximum sSize
                  amount = length $ filter (\x -> x == maxSize && maxSize /= 0) sSize
              in if maxSize == 0 then (0,1) else (maxSize, amount)

subStrings :: String -> [Int]
subStrings [] = []
subStrings s@(c:str) = isValidUntil s 0 0 : subStrings str

isValidUntil :: String -> Int -> Int -> Int
isValidUntil [] acc l = if acc == 0 then l else 0
isValidUntil ('L':str) 0 l = if next > 0 then next else l
  where next = isValidUntil str 0 l
isValidUntil ('R':str) 0 l = if next > 0 then next else l
  where next = isValidUntil str 0 l
isValidUntil ('R':str) acc l = isValidUntil str (acc+1) (l + 1)
isValidUntil ('L':str) acc l = isValidUntil str (acc-1) (l + 1)


-- "RRLLRRLLR"
-- 1 1
-- 2 2
-- 1 3
-- 0 3
-- 


path1v = "RRRLRLLL"
path2v = "RRLL"
path3i = "RRLRRLLL"
path4i = "LR"

path5o = "RLLR"

-- LRRRLLLLRRLRLL
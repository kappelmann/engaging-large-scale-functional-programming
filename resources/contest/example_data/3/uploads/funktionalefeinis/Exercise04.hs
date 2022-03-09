module Exercise04 where

data Dir = L | R
  deriving (Show, Eq, Read)

getDir :: Char -> Dir
getDir 'L' = L
getDir 'R' = R

rudolph :: String -> (Int, Int)
rudolph s = case helper s 0 0 of
                (0,_) -> (0,1)
                t -> t

         --dirs   index  length  maxDepth
helper :: String -> Int -> Int -> (Int, Int)
helper [] 0 l = (l, 1)
helper [] _ l = (0, 1)
helper ('L':ls) 0 l
--  | trace ("return " ++ show l ++ " " ++ show rest) False = undefined
  | l == fst rest = {-trace (show (l, 1 + snd rest))-} (l, 1 + snd rest)
  | l > fst rest = (l, 1)
  | otherwise = rest
    where rest = helper ls 0 0
helper ('R':ls) 0 l
  | l == fst rest = (l, 1 + snd rest)
  | l > fst rest = (l, 1)
  | otherwise = rest
    where rest = helper ls 1 (l+1)
helper ('R':ls) i l
--  | trace ("helper R " ++ show one ++ " " ++ show two) False = undefined
  | fst one == fst two = {-trace ("helper R return " ++ show (fst one, max (snd one) (snd two)))-} (fst one, max (snd one) (snd two))
  | fst one > fst two = {-trace ("helper R return " ++ show one)-} one
  | otherwise = {-trace ("helper R return " ++ show two)-} two
    where
      one = helper ls (i+1) (l+1)
      two = helper ls 1 1
helper ('L':ls) i l = helper ls (i-1) (l+1)

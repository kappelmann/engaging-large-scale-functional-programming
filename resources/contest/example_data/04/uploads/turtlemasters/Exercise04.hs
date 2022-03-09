module Exercise04 where

import Data.List
import Data.Ord
rudolph :: String -> (Int, Int)
rudolph s = if null strings then (0,1) else (length sol, length strings)
    where
          strings = filter isCorrect $ subsequences s
          sol = maximumBy (comparing length) strings

isCorrect :: String -> Bool
isCorrect = isCorrectAcc 0


isCorrectAcc 0 [] = True
isCorrectAcc 0 (x:xs) = x /= 'L' && isCorrectAcc 1 xs
isCorrectAcc n (x:xs) = if x == 'L' then isCorrectAcc (n-1) xs else isCorrectAcc (n+1) xs
isCorrectAcc _ _ = False

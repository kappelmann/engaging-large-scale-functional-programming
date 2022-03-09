module Exercise04 where

import Data.List (maximumBy, nub, isInfixOf)
--import Test.QuickCheck

rudolph :: String -> (Int, Int) 
rudolph s = let rlens = rudolph' s 0
                mx = last rlens
            in if null rlens || mx == 0 then (0, 1)
               else (mx, length (dropWhile (mx /=) rlens))

evalChar :: Char -> (Int -> Int)
evalChar 'R' = (+ 1)
evalChar 'L' = \i -> i - 1

evalWhileValid :: String -> Int -> Int -> Int -> (Int, Bool)
evalWhileValid [] done lastZero p = (lastZero, lastZero /= 0)
evalWhileValid (x:xs) done lastZero p = let res = evalChar x p
                                        in if res < 0 then (lastZero, lastZero /= 0)
                                           else if res == 0 then evalWhileValid xs (1+done) (1+done) res
                                                else evalWhileValid xs (1+done) lastZero res

rudolph' :: String -> Int -> [Int]
rudolph' [] _ = []
rudolph' str@(x:xs) currentMax = let (n, b) = evalWhileValid str 0 0 0
                                 in if b && n >= currentMax then n : rudolph' xs n
                                    else rudolph' xs currentMax

-- Testing
validSubsequence :: String -> Int -> Bool
validSubsequence [] p = p == 0
validSubsequence (x:xs) p = let r = evalChar x p in if r < 0 then False else validSubsequence xs r

subsequences' :: [a] -> [[a]]
subsequences' [] = []
subsequences' str@(x:xs) = [ take i str | i <- [1..length str]] ++ subsequences' xs

inefficientRudolph :: String -> (Int, Int)
inefficientRudolph s = let subs = filter (\str -> str `validSubsequence` 0 && str `isInfixOf` s) (subsequences' s)
                           lens = map length subs
                           mx = maximum lens
                       in if null subs || mx == 0 then (0, 1)
                          else (mx, length (filter (mx ==) lens))

data RudolphLetter = R | L
  deriving Show

--instance Arbitrary RudolphLetter where
--    arbitrary = oneof [return R, return L]

rsToString :: [RudolphLetter] -> String
rsToString [] = []
rsToString (R:rs) = 'R' : rsToString rs
rsToString (L:rs) = 'L' : rsToString rs

prop_matches :: [RudolphLetter] -> Bool
prop_matches rs = let str = rsToString rs in rudolph str == inefficientRudolph str
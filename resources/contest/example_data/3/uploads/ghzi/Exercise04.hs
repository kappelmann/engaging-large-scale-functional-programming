module Exercise04 where

import Data.Maybe

rudolph :: String -> (Int, Int)
rudolph s
	-- ~ | isNothing mb_ps = (0, 1)
	| null ss = (0, 1)
	| otherwise = (maxi, count)
	where
		-- ~ mb_ps = preprocess s
		ps = preprocess s
		reduceds = reduce ps
		ss = map (\(S n) -> n) $ filter isS reduceds
		maxi = maximum ss
		count = length [s | s <- ss, s == maxi]

isS (S _) = True
isS _ = False

-- R=(, L=)
data LR = L | R | S Int deriving (Show, Eq)

-- ~ preprocess :: String -> Maybe [LR]
-- ~ preprocess [] = Just []
-- ~ preprocess ('R':xs) = do
	-- ~ rs <- preprocess xs
	-- ~ Just (R : rs)
-- ~ preprocess ('L':xs) = do
	-- ~ rs <- preprocess xs
	-- ~ Just (L : rs)
-- ~ preprocess otherwise = Nothing
preprocess :: String -> [LR]
preprocess [] = []
preprocess ('R':xs) = R : preprocess xs
preprocess ('L':xs) = L : preprocess xs
preprocess (x:xs) = preprocess xs

reduceOnce :: [LR] -> [LR]
reduceOnce [] = []
reduceOnce (R:L:xs) = S 2 : reduceOnce xs
reduceOnce (R:(S n):L:xs) = S (n+2) : reduceOnce xs
reduceOnce ((S n1):(S n2):xs) = S (n1 + n2) : reduceOnce xs
reduceOnce (x:xs) = x : reduceOnce xs

reduce :: [LR] -> [LR]
reduce xs
	| xs == nexts = xs
	| otherwise = reduce nexts
	where
		nexts = reduceOnce xs

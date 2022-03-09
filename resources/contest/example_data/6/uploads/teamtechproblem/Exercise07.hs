module Exercise07 where

import Control.Monad (replicateM)

-- algorithm from https://stackoverflow.com/a/4294326/8602872
import Data.List(sort)
import Data.Set (Set, member, fromList, insert, union)

data Edge a = Edge a a Double deriving Show

instance (Eq a) => Eq (Edge a) where
  Edge x1 y1 z1 == Edge x2 y2 z2 = x1 == x2 && y1 == y2 && z1 == z2

instance Eq a => Ord (Edge a) where
  (Edge _ _ x) `compare` (Edge _ _ y) = x `compare` y

kruskal :: Ord a => [Edge a] -> [Edge a]
kruskal = fst . foldl mst ([],[]) . sort

mst :: Ord a => ([Edge a],[Set a]) -> Edge a -> ([Edge a],[Set a])
mst (es, sets) e@(Edge p q _) = step $ extract sets where
   step (rest, Nothing, Nothing) = (e : es, fromList [p,q] : rest)
   step (rest, Just ps, Nothing) = (e : es, q `insert` ps : rest)
   step (rest, Nothing, Just qs) = (e : es, p `insert` qs : rest)
   step (rest, Just ps, Just qs) | ps == qs = (es, sets) --circle
                                 | otherwise = (e : es, ps `union` qs : rest)
   extract = foldr f ([], Nothing, Nothing) where
       f s (list, setp, setq) =
            let list' = if member p s || member q s then list else s:list
                setp' = if member p s then Just s else setp
                setq' = if member q s then Just s else setq
            in (list', setp', setq')

-- Read space seperated words on a line from stdin
readMany :: Read a => IO [a]
readMany = fmap (map read . words) getLine

-- Adapted from https://stackoverflow.com/questions/8366093/how-do-i-parse-a-matrix-of-integers-in-haskell
parse :: IO (Int, [[Int]])
parse = do
  n      <- readLn :: IO Int
  xss    <- replicateM n readMany
  return (n, xss)

printGreetings :: [(Int,Int, Int)] -> IO ()
printGreetings [] = do
  return ()
printGreetings ((_,a,b):xs) = do
  putStrLn (show a ++ " " ++ show b)
  printGreetings xs

adjToList i [] = []
adjToList i (xs:xss) = [(i,j,w) | (j, w) <- zip [1..i] xs] ++ adjToList (succ i) xss

main = do
  (n, xss) <- parse
  let adjassoc = adjToList 1 xss
  --let g = mkGraph False (1,n) adjassoc
  --let mst = kprim g
  let mst = kruskal [Edge i j (fromIntegral w) | (i,j,w) <- adjassoc]
  printGreetings $ take (n-1) [(floor w,i,j) | (Edge i j w) <- mst]



{-
5
0 6 7 7 3
6 0 5 4 7
7 5 0 7 4
7 4 7 0 7
3 7 4 7 0
-}

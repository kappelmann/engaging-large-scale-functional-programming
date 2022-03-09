module Exercise05 where
















import qualified Data.Map.Strict as M (Map, member, insert, empty, singleton, (!), notMember, lookup, fromList)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import GHC.List (uncons)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Bifunctor (second, Bifunctor (first))
import qualified Data.Sequence as Seq (Seq, singleton, null, ViewL ((:<)), viewl, (><), fromList)
import Data.List (foldl')

type Pos = (Int,Int)

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

key :: Int -> Pos -> Int
key w (r, c) = r * w + c

neighbours :: Int -> Int -> [Pos] -> Pos -> Pos -> M.Map Pos [Pos]
neighbours h w rocks start flag@(fr, fc) = ns0 M.empty [start]
  where ns0 m [] = m
        ns0 m (pos:poss)
          | pos == flag       = ns0 m poss
          | pos `M.member` m  = ns0 m poss
          | otherwise         = ns0 (M.insert pos newPositions m) (newPositions ++ poss)
              where newPositions = [pos' | d <- [leftOf, rightOf, above, below], let pos' = d pos, pos' /= pos]

        sameRow (r, _) = filter ((== r) . fst) rocks
        sameCol (_, c) = filter ((== c) . snd) rocks

        leftOf p@(r, c) = let res@(rr, rc) = maybe (r, 0) (second (+1) . fst) $ uncons (sortOn (Down . snd) $ filter ((< c) . snd) $ sameRow p) in if fr == r && fc > rc && fc < c then flag else res
        rightOf p@(r, c) = let res@(rr, rc) = maybe (r, w - 1) (second (subtract 1) . fst) $ uncons (sortOn snd $ filter ((> c) . snd) $ sameRow p) in if fr == r && fc < rc && fc > c then flag else res
        above p@(r, c) = let res@(rr, rc) = maybe (0, c) (first (+1) . fst) $ uncons (sortOn (Down . fst) $ filter ((< r) . fst) $ sameCol p) in if fc == c && fr > rr && fr < r then flag else res
        below p@(r, c) = let res@(rr, rc) = maybe (h - 1, c) (first (subtract 1) . fst) $ uncons (sortOn fst $ filter ((> r) . fst) $ sameCol p) in if fc == c && fr < rr && fr > r then flag else res

dfs :: Pos -> Pos -> M.Map Pos [Pos] -> Int
dfs start end graph = fromMaybe (-1) $ M.lookup end $ dfs0 (M.singleton start 0) (Seq.singleton start)
  where dfs0 :: M.Map Pos Int -> Seq.Seq Pos -> M.Map Pos Int
        dfs0 d q
          | Seq.null q  = d
          | u == end    = d
          | otherwise   = dfs0 (foldl' withPos d newNs) (q' Seq.>< Seq.fromList newNs)
            where (u Seq.:< q') = Seq.viewl q
                  du = d M.! u
                  ns = graph M.! u
                  newNs = filter (`M.notMember` d) ns
                  withPos map pos = M.insert pos (du + 1) map

        x = dfs0 (M.singleton start 0) (Seq.singleton start)
        x2 = M.lookup end $ dfs0 (M.singleton start 0) (Seq.singleton start)

incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag = dfs start flag $ neighbours h w rocks start flag

-- let { h = 10; w = 10; rocks = [(7,2),(3,3),(4,9)]; start = (7,6); flag@(fr, fc)=(4,7) }
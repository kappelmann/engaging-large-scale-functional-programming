module Exercise07 where

import Text.Read ( readMaybe )
import Data.List (sortOn)
































import qualified Data.Map.Strict as M (Map, singleton, null, findMin, deleteMin, updateMin, (!), adjust, alter, keys, filterWithKey, lookup, member, insert, foldl, foldlWithKey, foldlWithKey', empty, toList, fromList)
import Data.Function (on)

import Prelude hiding (null)
import Data.Maybe (fromMaybe)

type PriorityQueue a = M.Map Int [a]

singleton :: Int -> a -> PriorityQueue a
singleton k v = M.singleton k [v]

null :: PriorityQueue a -> Bool
null = M.null

notNull :: PriorityQueue a -> Bool
notNull = not . M.null

deleteMin :: PriorityQueue a -> (a, PriorityQueue a)
deleteMin pq = case snd $ M.findMin pq of
                  [] -> error "internal error"
                  [x] -> (x, M.deleteMin pq)
                  (x:_) -> (x, M.updateMin (Just . tail) pq)

decreaseKey :: Eq a => Int -> Int -> a -> PriorityQueue a -> PriorityQueue a
decreaseKey oldW newW el pq 
  | newW < oldW = M.alter ins newW $ M.alter del oldW pq
  | otherwise   = pq

    where del Nothing = Nothing
          del (Just els) = case els' of
                            [] -> Nothing
                            _ -> Just els'
            where els' = filter (/= el) els
      
          ins Nothing = Just [el]
          ins (Just els) = Just $ el:els

contains :: Eq a => Int -> a -> PriorityQueue a -> Bool
contains w el pq = el `elem` pq M.! w

main :: IO ()
main = do
    line <- getLine
    case readMaybe line :: Maybe Int of
        Nothing -> putStrLn "Could not read input. Terminating..."
        Just x  -> do
            matrix <- readMatrix x []
            -- let shortestCalls = shortestCalls matrix
            outputShortestCalls $ shortestCalls matrix x

outputShortestCalls :: [(Int, Int)] -> IO ()
outputShortestCalls [] = putStr ""
outputShortestCalls [(i, j)] = putStrLn (show i ++ " " ++ show j)
outputShortestCalls ((i, j):xs) = putStrLn (show i ++ " " ++ show j) >> outputShortestCalls xs

-- outputIntMatrix :: [[(Int, Int)]] -> IO ()
-- outputIntMatrix [] = return ()
-- outputIntMatrix (xs:xss) = do out

readMatrix :: Int -> [[Int]] -> IO [[Int]]
readMatrix 0 matrix = return matrix
readMatrix n matrix = do
    line <- getLine
    row <- transformRow line
    readMatrix (n - 1) (matrix ++ [row])

transformRow :: String -> IO [Int]
transformRow input = return (map read split)
    where split = words input

shortestCalls :: [[Int]] -> Int -> [(Int, Int)]
shortestCalls matrix dimensions =
    let ijs = [(i,j) | i <- [1..dimensions], j <- [1..dimensions]]
        edges = zip ijs (concat matrix)
        sortedEdges = sortOn snd edges
    in M.toList $ jarnikPrim $ M.fromList sortedEdges


jarnikPrim :: M.Map (Int, Int) Int {- -> PriorityQueue (Int, Int) -} -> M.Map Int Int
jarnikPrim map = jp (M.singleton startNode 0) M.empty (singleton 0 $ fst startNode)
    where startNode = fst $ M.findMin map
          ns = M.keys map
          jp d p pq
            | null pq     = p
            | otherwise   = jp d' p' pq''
                where (v, pq') = deleteMin pq
                      allvw = M.filterWithKey (\(from, _) _ -> from == v) map
                      ifvw = M.filterWithKey (\e@(from, to) w -> maybe True (< w) (M.lookup e d) && M.member w p) allvw
                      p' = M.foldlWithKey' (\p1 (from, to) _ -> M.insert to from p1) p ifvw
                      pq'' = M.foldlWithKey' (\pq1 e@(from, to) w -> decreaseKey (fromMaybe 0 $ M.lookup e d) w to pq1) pq' ifvw
                      d' = M.foldlWithKey' (\d1 e@(from, to) w -> M.insert e w d1) d ifvw


    --  otherwise     =
    --     let v = pq.deleteMin
    --         v_edges = [(v, w) | w <- [1..dimension]]
    --         v_edges_weight = map (\(v, w) -> ((v, w), M.lookup (v, w) map))



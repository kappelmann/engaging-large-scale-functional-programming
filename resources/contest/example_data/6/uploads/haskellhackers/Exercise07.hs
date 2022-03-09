module Exercise07 where

import Data.IntMap (IntMap, fromList, lookup, insert)
import Data.Maybe (fromJust)
import qualified Data.List as DL

-- Jarnik-Prim, GAD slide 596

solveMatrix :: [[Int]] -> Int -> [(Int, Int)]
solveMatrix mx n = map (\(a,b) -> (a+1,b+1)) $ convert finishedPred n
    where
        finishedPred = whileLoop pq pred distance

        convert :: IntMap (Maybe Int) -> Int -> [(Int, Int)]
        convert pred 0 = []
        convert pred n = case fromJust $ Data.IntMap.lookup (n-1) pred of
                            Nothing -> convert pred (n-1)
                            Just v -> (v,n-1):convert pred (n-1)

        distance :: IntMap Int
        distance = fromList pq

        pred :: IntMap (Maybe Int)
        pred = fromList $ take n $ zip [0..] $ repeat Nothing
        pq :: [(Int, Int)] -- node, costs
        pq = (0,0) : zip [1..] (replicate (n-1) (maxBound :: Int))

        costs :: Int -> Int -> Int
        costs v w = mx !! v !! w

        whileLoop :: [(Int,Int)] -> IntMap (Maybe Int) -> IntMap Int -> IntMap (Maybe Int)
        whileLoop [] pred _ = pred
        whileLoop ((v,cost):pq) pred d = whileLoop newpq newpred newd
            where
                (newpq, newpred, newd) =  forAll [(v,w) | w<- [0..(n-1)],v /= w] pq pred d
                                
        forAll :: [(Int,Int)] -> [(Int,Int)] -> IntMap (Maybe Int) -> IntMap Int -> ([(Int, Int)], IntMap (Maybe Int), IntMap Int)
        forAll [] pq pred d = (pq, pred, d)
        forAll ((v,w):edges) pq pred d = if not (newWeight < dw && w `elem` map fst pq) then forAll edges pq pred d else forAll edges newpq newpred newd
            where
                newWeight = costs v w
                dw = fromJust $ Data.IntMap.lookup w d
                newpq = DL.insertBy (\(_,wa) (_,wb) -> compare wa wb) (w, newWeight) $ filter ((/= w) . fst) pq
                newpred = Data.IntMap.insert w (Just v) pred
                newd = Data.IntMap.insert w newWeight d
        
testGiven = solveMatrix [[0,6,7,7,3],[6,0,5,4,7],[7,5,0,7,4],[7,4,7,0,7],[3,7,4,7,0]] 5

testArtemis = solveMatrix [[0, 1795],[1795,0]] 2

main :: IO ()
main = do 
        n <- readLn :: IO Int
        matrix <- getMatrix n
        let conns = solveMatrix matrix n
        printConns conns
        
        where
            printConns :: [(Int, Int)] -> IO ()
            printConns [] = return ()
            printConns ((l,r):conns) = do   putStrLn $ show l ++ " " ++ show r
                                            printConns conns
            
            getMatrix :: Int -> IO [[Int]]
            getMatrix 0 = return []
            getMatrix n = do
                            line <- getLine 
                            let ints :: [Int]
                                ints = map read $ words line
                            rem <- getMatrix (n-1)
                            return (ints : rem)



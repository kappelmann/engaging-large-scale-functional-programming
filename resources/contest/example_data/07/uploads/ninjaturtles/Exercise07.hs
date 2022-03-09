module Exercise07 where

main :: IO ()
main = do
    n <- readLn
    -- print n
    graph <- castMatrix n
    let maxVal = maximum (map maximum graph) + 1
        updatedGraph = setSelfToMax maxVal graph
        mstEdgeList = mst maxVal updatedGraph
        mathList = map (\(x, y) -> (x+1, y+1)) mstEdgeList
    -- print updatedGraph
    outputList mathList
    return ()
    -- Debugging

outputList :: [(Int, Int)] -> IO()
outputList [] = return()
outputList ((i, j):rs) = do putStrLn $ show i ++ " " ++ show j
                            outputList rs

testMat :: [[Int]]
testMat = [[8,6,7,7,3],[6,8,5,4,7],[7,5,8,7,4],[7,4,7,8,7],[3,7,4,7,8]]

setSelfToMax :: Int -> [[Int]] -> [[Int]]
setSelfToMax maxVal mat = foldl (\currMat i -> updateAdjMatrix maxVal i [i] currMat) mat [0..(n - 1)]
    where n = length mat

-- Calculates the minimal spanning tree from a graph given by its adjacency matrix
mst :: Int -> [[Int]] -> [(Int, Int)]
mst maxVal graph = mstRec maxVal [1] [] graph $ length graph

-- TODO Garantiere Zusammenhängigkeit
mstRec :: Int -> [Int] -> [(Int, Int)] -> [[Int]] -> Int -> [(Int, Int)]
mstRec maxVal currMembers currEdges adjMatrix n
    | length currMembers >= n   = currEdges
    | otherwise                 = mstRec maxVal newJMembers newEdges newAdjMatJ n
    where   ((i, j), _) = getMinMat maxVal adjMatrix currMembers -- Only edges from currMembers are allowed - TODO
            newEdges = (i, j):currEdges
            (addedI, newIMembers) = addUnique i currMembers
            (addedJ, newJMembers) = addUnique j newIMembers
            newAdjMatI = if addedI then updateAdjMatrix maxVal i newJMembers adjMatrix else adjMatrix
            newAdjMatJ = if addedJ then updateAdjMatrix maxVal j newJMembers newAdjMatI else newAdjMatI

-- replaces all edges from n with others with maxVal
updateAdjMatrix :: Int -> Int -> [Int] -> [[Int]] -> [[Int]]
updateAdjMatrix maxVal i others mat = updated 
    where   -- updates the i-th row
            updateRow = foldl (\newXs k -> updateList k maxVal newXs) (mat !! i) others
            -- updates the i-th col and adds the new i-th row
            updated = foldl (\currMat r -> updateMatRowRec 0 r (if r == i then const updateRow else updateList i maxVal) currMat) mat others
            
updateMatRowRec :: Int -> Int -> ([Int] -> [Int]) -> [[Int]] -> [[Int]]
updateMatRowRec k i f [] = []
updateMatRowRec k i f (xs:xss)
    | k < i     = xs : updateMatRowRec (k + 1) i f xss
    | k == i    = f xs : xss
    | otherwise = undefined 


updateList :: Int -> Int -> [Int] -> [Int]
updateList = updateListRec 0

updateListRec :: Int -> Int -> Int -> [Int] -> [Int]
updateListRec k i newEle [] = []
updateListRec k i newEle (x:xs)
    | k < i     = x : updateListRec (k + 1) i newEle xs
    | k == i    = newEle : xs
    | otherwise = undefined 

addUnique :: Int -> [Int] -> (Bool, [Int])
addUnique x xs
    | x `elem` xs   = (False, xs)
    | otherwise     = (True, x:xs)

getMinMat :: Int -> [[Int]] -> [Int] -> ((Int, Int), Int)
getMinMat maxVal xss alloweds = getMinMatRec 0 minLists ((-1, -1), maxVal) alloweds
    where minLists = map (\xs -> getMinListRec 0 xs (-1, maxVal)) xss

getMinMatRec :: Int -> [(Int, Int)] -> ((Int, Int), Int) -> [Int] -> ((Int, Int), Int)
getMinMatRec _ [] currMin _ = currMin
getMinMatRec currRow ((minInd, minVal):rs) currMin@(_, currMinVal) alloweds = getMinMatRec (currRow + 1) rs (if minVal < currMinVal && (currRow `elem` alloweds) then ((currRow, minInd), minVal) else currMin) alloweds

getMinListRec :: Int -> [Int] -> (Int, Int) -> (Int, Int)
getMinListRec currInd [] currMin = currMin
getMinListRec currInd (x:xs) currMin@(minIndex, minValue) = getMinListRec (currInd + 1) xs $ if x < minValue then (currInd, x) else currMin

castMatrix :: Int -> IO[[Int]]
castMatrix n = do
    if n == 0 then
        return []
    else
        do
            input <- getLine
            let xs = reverse $ splitOn input [[]]
            xss <- castMatrix (n-1)
            return (xs : xss)

splitOn :: String -> [String] -> [Int]
splitOn [] ys = map (read . reverse) ys
splitOn (x:xs) ys
    | x /= ' ' = splitOn xs ((x:head ys): tail ys)
    | x == ' ' = splitOn xs ([]:ys)


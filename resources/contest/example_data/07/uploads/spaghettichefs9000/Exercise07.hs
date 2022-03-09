module Exercise07 where

type Graph = [[Int]]

getInput :: IO [[Int]]
getInput = do line <- getLine
              let n = read line :: Int
              getInputAux 0 n []


getInputAux :: Int -> Int -> [[Int]] -> IO [[Int]]
getInputAux k n acc = if   k == n
                   then return (reverse acc)
                   else do line <- getLine
                           getInputAux (k + 1) n (line2List line : acc)

line2List :: String -> [Int]
line2List line = map read (words line)

main :: IO ()
main = do
  graph <- getInput 
  let result = snd (findShortest graph)
  putResult result
 
putResult :: [(Int,Int)] -> IO ()
putResult [] = return ()
putResult ((n1,n2):xs) = do
  putStr (show n1 ++ " " ++ show n2 ++ "\n")
  putResult xs

findShortest :: Graph -> (Int, [(Int, Int)])
findShortest g = findShortestHelper g [1] [2..length g] 0 []

findShortestHelper :: Graph -> [Int] -> [Int] -> Int -> [(Int, Int)] -> (Int, [(Int, Int)])
findShortestHelper g visited [] l steps = (l, steps)
findShortestHelper g visited unvisited l steps = findShortestHelper g (toNode : visited) (remFromList toNode unvisited) (l + stepL) ((fromNode, toNode) : steps)
  where
    nS = nextStep g visited unvisited
    fromNode = fst nS
    toNode = fst (snd nS)
    stepL = snd (snd nS)

remFromList :: Int -> [Int] -> [Int]
remFromList _ [] = []
remFromList x (y : ys)
  | x == y = remFromList x ys
  | otherwise = y : remFromList x ys

-- nextStep -- (fromNode, (toNode, length)))
nextStep :: Graph -> [Int] -> [Int] -> (Int, (Int, Int))
nextStep g visited unvisited = head allPossible
  where
    allNext = allShortestPathToAny g visited unvisited
    m = findMin allNext
    allPossible = filter (\(_, (_, l)) -> l == m) allNext

-- shortest from all nodes
allShortestPathToAny :: Graph -> [Int] -> [Int] -> [(Int, (Int, Int))]
allShortestPathToAny g visited unvisited = possiblePaths
  where
    possiblePaths = zip visited (map (\x -> shortestPathToAny g x unvisited) visited)

--- shortest from 1 node
shortestPathToAny :: Graph -> Int -> [Int] -> (Int, Int)
shortestPathToAny g node unvisited
  | null minNode = (-1, -1)
  | otherwise = head minNode
  where
    allPaths = zip [1, 2, 3, 4, 5] (map (\xs -> xs !! (node -1)) g)
    allValidPaths = filter (\(n, l) -> n /= node && elem n unvisited) allPaths
    minL = minimum (map snd allValidPaths) 
    minNode = filter (\x -> snd x == minL) allValidPaths

--- 
-- findMinNode :: [(Int,Int)] -> (Int,Int)
-- findMinNode

-- findMinNodeHelper :: [(Int,Int)] -> Int -> (Int,Int)
-- findMinNodeHelper 

-------- Helpers

findMin :: [(Int, (Int, Int))] -> Int
findMin options = findMinHelper  (tail options) (snd (snd (head options)))

findMinHelper :: [(Int, (Int, Int))] -> Int -> Int
findMinHelper [] l = l
findMinHelper (x:xs) l
  | next < l = findMinHelper xs next
  | otherwise = findMinHelper xs l
  where next = snd (snd x)




-- test = findShortest [[0, 6, 7, 7, 3], [6, 0, 5, 4, 7], [7, 5, 0, 7, 4], [7, 4, 7, 0, 7], [3, 7, 4, 7, 0]]

-- test2 = findShortest [[0, 1, 2], [1, 0, 2], [2, 2, 0]]

-- testS = shortestPathToAny [[0, 1, 2], [1, 0, 2], [2, 2, 0]] 1 [2, 3]

-- testS2 = allShortestPathToAny [[0, 6, 7, 7, 3], [6, 0, 5, 4, 7], [7, 5, 0, 7, 4], [7, 4, 7, 0, 7], [3, 7, 4, 7, 0]] [1] [2, 3, 4, 5] -- (5,3)

-- testS4 = allShortestPathToAny [[0, 6, 7, 7, 3], [6, 0, 5, 4, 7], [7, 5, 0, 7, 4], [7, 4, 7, 0, 7], [3, 7, 4, 7, 0]] [1] [2, 3, 4] -- (2,6)

-- testS5 = allShortestPathToAny [[0, 6, 7, 7, 3], [6, 0, 5, 4, 7], [7, 5, 0, 7, 4], [7, 4, 7, 0, 7], [3, 7, 4, 7, 0]] [1, 4] [2, 3, 4] -- (2,6)

-- testS3 = shortestPathToAny [[0, 6, 7, 7, 3], [6, 0, 5, 4, 7], [7, 5, 0, 7, 4], [7, 4, 7, 0, 7], [3, 7, 4, 7, 0]] 1 [2, 3, 4, 5]

-- testS8 = nextStep [[0, 6, 7, 7, 3], [6, 0, 5, 4, 7], [7, 5, 0, 7, 4], [7, 4, 7, 0, 7], [3, 7, 4, 7, 0]] [1] [2, 3, 4, 5]


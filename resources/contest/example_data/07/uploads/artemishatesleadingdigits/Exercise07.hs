module Exercise07 where

import Data.List

main :: IO ()
main = do
    num <- getLine 
    let num' = read num :: Int
    -- matrix is built
    matrix <- getInput num'
    
    -- transivity, reflexivity

    -- searching for the minimal sound level
    let (x:reducedMat) = reduceMatrix matrix 0
        sortedReducedMat = sortOn fst (transfer reducedMat 2) 
    
    printS (map snd (take (num' - 1) sortedReducedMat))
    return ()

    where 
        getInput :: Int -> IO [[Int]]
        getInput 0 = return []
        getInput num' =do
            line <- getLine 
            let matline = map (\x -> read x :: Int) (words line)
            nextLine <- getInput (num'-1)
            return (matline : nextLine)

printS :: [String] -> IO ()
printS [] = return ()
printS (x:xs) = do
    putStrLn x 
    printS xs
    return ()

-- reducedMatrix as input
-- [(a,b)]: people
transfer :: [[Int]] -> Int -> [(Int, String)]
transfer [] _ = []
transfer (xs:xxs) n = transferLine xs n 1 ++ transfer xxs (n+1)
    where 
        transferLine [] _ _ = []
        transferLine (a:ax) n m = (a, show m ++ " " ++ show n) : transferLine ax n (m+1)


-- get only the left part of the matrix
reduceMatrix :: [[Int]] -> Int -> [[Int]] 
reduceMatrix [] _ = []
reduceMatrix (x:xs) n = reduceLine x n : reduceMatrix xs (n+1)
    where 
        reduceLine (a:ax) n 
            | n == 0 = []
            | otherwise = a : reduceLine ax (n-1)

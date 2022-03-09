module Exercise07 where

import Data.List

main :: IO ()
main = do
        nRaw <- getLine
        readLines (toInt nRaw) []
readLines :: Int -> [[Int]] -> IO ()

readLines 0 distances = do
                          printGreetings (helper distances)
readLines n distances = do
                          lineRaw <- getLine
                          readLines (n-1) (distances ++ [map toInt (words lineRaw)])

printGreetings :: [(Int, Int)] -> IO ()
printGreetings [] = print " "
printGreetings [(a,b)] = print (show a ++  " " ++ show b)
printGreetings ((a,b):gs) = do
                              print (show a ++  " " ++ show b)
                              printGreetings gs

toInt raw = read raw::Int


helper :: [[Int]] -> [(Int, Int)]
helper distances = let (i, j) = getFirst (head shorts) in
                    recHelper (filterer [i, j] (tail shorts)) [(i, j)] [i, j] (length distances)
                      where getFirst (i, j, _) = (i, j)
                            shorts = getShorts distances
                            recHelper ss greetings greeted l = let (ni, nj, _) = head ss in
                                                                if l <= length greeted then greetings
                                                                else recHelper (filterer (greeted ++ [ni, nj]) (tail shorts)) (greetings ++ [(ni, nj)]) (greeted ++ [ni, nj]) l
                            filterer greeted = filter (\(a, _, _) -> a `notElem` greeted)

getShorts :: [[Int]] -> [(Int, Int, Int)]
getShorts distances = let l = length distances in 
                        filter (\(_,_,d) -> d /= 0) (sortOn (\(_,_,d) -> d) (concatMap (zipper l) (zip [1 .. l] distances)))
                          where zipper l (index, ds) = map (\(j, d) -> (index, j, d)) (zip [1 .. l] ds)
          
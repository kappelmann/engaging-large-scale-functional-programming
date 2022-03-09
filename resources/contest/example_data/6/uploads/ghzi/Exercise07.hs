module Exercise07 where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.IntSet as IntSet

main :: IO ()
main = do x <- getLine
          let n = read x
          matrix <- readMatrix n
          let greets = calculateGreetings n matrix
          printGreets greets


readMatrix :: Int -> IO [[Int]]
readMatrix 0 = return []
readMatrix n = do x <- getLine
                  let y = map read (words x)
                  z <- readMatrix (n-1)
                  return (y : z)


calculateGreetings :: Int -> [[Int]] -> [(Int, Int)]
--calculateGreetings n xs = calculateGreetingsHelper [1] (zip [1 .. n] (map (\x -> zip [1 .. n] x) xs))
calculateGreetings n xs = calculateGreetingsHelper2 (IntSet.singleton 1) gesamt
  where
      gesamt = map fst (sortOn (snd) gesamtUnsorted)
      gesamtUnsorted = tail (concatMap (\(num1, x) -> map (\(num2, y) -> ((num1, num2), y)) x) (zip [1 .. n] (map (\x -> (zip [1 .. n] x)) xs)))

calculateGreetingsHelper :: [Int] -> [(Int, [(Int, Int)])] -> [(Int, Int)]
calculateGreetingsHelper zs [] = []
calculateGreetingsHelper zs xs = if null zusammenhangskomponente then [] else (von, zu) : calculateGreetingsHelper (zu:zs) xs
    where
        zusammenhangskomponente = filter (\(num, x)-> not (null x)) (map (\(num, x) -> (num, filter (\(num2, _) -> not (num2 `elem` zs)) x)) (filter (\(num, _) ->num `elem` zs) xs))

        zusammenhangskomponente2 = map (\(num, x) -> (num, minimumBy (\x y -> comparing snd x y) x)) zusammenhangskomponente
        (von, (zu, _)) = minimumBy (\x y -> comparing (snd . snd) x y) zusammenhangskomponente2




calculateGreetingsHelper2 :: IntSet.IntSet -> [(Int, Int)] -> [(Int, Int)]
calculateGreetingsHelper2 zs [] = []
calculateGreetingsHelper2 zs xs = (von, zu) : calculateGreetingsHelper2 zsNeu xsNeu
    where
        xsNeu = filter (\(von, zu) -> not (zu `IntSet.member` zsNeu)) xs
        zsNeu = IntSet.insert zu zs
        (von, zu) = fromMaybe (0, 0) (find (\(von, zu)-> von `IntSet.member` zs) xs)



printGreets :: [(Int, Int)] -> IO ()
printGreets [] = return ()
printGreets ((von, zu) : xs) = do putStrLn (show von ++ " "++ show zu)
                                  printGreets xs
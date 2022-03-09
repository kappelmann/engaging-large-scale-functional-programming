module Exercise07 where

import Control.Monad (forM_, forM)
import qualified Data.Set as S

main :: IO ()
main = do
  h <- getLine
  let n = read h :: Int
  r <-
    mapM
      ( \i ->
          do
            l <- getLine
            let w =
                  map (\(x, j) -> (read x:: Int, (i, j))) $
                    filter (\(_, j) -> j /= i) $ zip (words l) [0 .. n - 1]
            return w
      )
      [0 .. n - 1]
  let as = primAlgorithm r
  forM_ as (\(i, j) -> putStrLn (show i ++ " " ++ show j))

primAlgorithm :: Show a => Ord a => [[(a, (Int, Int))]] -> [(Int, Int)]
primAlgorithm (xs : xss) = go (S.fromList xs) (S.singleton 0)
  where
    go set doneSet
      | S.null (set) = []
      | otherwise = (i + 1, j + 1) : go next newDoneSet
      where
        ((_, (i, j)), rest) = S.deleteFindMin set
        newDoneSet =  S.insert j doneSet
        next = S.filter (\(_, (_, j')) -> j' `S.notMember` newDoneSet) (S.union rest (S.fromList ((xs : xss) !! j)))





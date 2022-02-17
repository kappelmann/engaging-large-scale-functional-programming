module Sudokus where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen
import Text.Printf (printf)

import qualified Solution as Sol

showSudoku :: [[Int]] -> String
showSudoku xss = unlines $ intercalate [showDivider] $ chunksOf squareSize $ map showRow xss
  where
    size = length xss
    squareSize = floor . sqrt . fromIntegral $ fromIntegral size 
    numberSize = size `div` 10 + 1

    showRowSection xs = unwords $ map (printf ("%0" ++ show numberSize ++ "d")) xs
    showRow xs = intercalate "|" $ map showRowSection $ chunksOf squareSize xs
    showDivider = intercalate "+" $ replicate squareSize $ replicate ((numberSize + 1) * squareSize - 1) '-'

    chunksOf :: Int -> [e] -> [[e]]
    chunksOf i [] = []
    chunksOf i ls = take i ls : chunksOf i (drop i ls)

emptySudoku n = replicate (n * n) (replicate (n * n) 0)

chooseCell :: (Int, Int) -> Gen ((Int, Int), Int)
chooseCell (l, u) = do
  a <- choose (l, u)
  b <- choose (l, u)
  c <- choose (l, u)
  return ((a - 1, b - 1), c)

newtype GenSparseSudoku = GenSparseSudoku [[Int]]
  deriving (Eq)

instance Show GenSparseSudoku where
  show (GenSparseSudoku s) = showSudoku s

instance Arbitrary GenSparseSudoku where
  arbitrary = scale (max 1) $ do
    n <- getSize
    cs <- take n <$> infiniteListOf (chooseCell (1, n * n))
    return $ GenSparseSudoku $ foldr (\(c, v) s -> Sol.setCell s c v) (emptySudoku n) cs

newtype GenDenseSudoku = GenDenseSudoku [[Int]]
  deriving (Eq)

instance Show GenDenseSudoku where
  show (GenDenseSudoku s) = showSudoku s

instance Arbitrary GenDenseSudoku where
  arbitrary = scale (max 1) $ do
    n <- getSize
    cs <- take (n * n) <$> infiniteListOf (chooseCell (1, n * n)) 
    return $ GenDenseSudoku $ foldr (\(c, v) s -> Sol.setCell s c v) (emptySudoku n) cs

hard :: [[Int]]
hard = [[8,0,0,0,0,0,0,0,0],
        [0,0,3,6,0,0,0,0,0],
        [0,7,0,0,9,0,2,0,0],
        [0,5,0,0,0,7,0,0,0],
        [0,0,0,0,4,5,7,0,0],
        [0,0,0,1,0,0,0,3,0],
        [0,0,1,0,0,0,0,6,8],
        [0,0,8,5,0,0,0,1,0],
        [0,9,0,0,0,0,4,0,0]]

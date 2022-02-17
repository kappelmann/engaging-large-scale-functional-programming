module Exercise03 where

import Text.Printf (printf)
import Data.List (intercalate)

-- HA 3.1a) i
selectRow :: [[Int]] -> Int -> [Int]
selectRow xss i = undefined

-- HA 3.1a) ii
selectColumn :: [[Int]] -> Int -> [Int]
selectColumn xss i = undefined

-- HA 3.1a) iii
intRoot :: Int -> Int
intRoot = floor . sqrt . fromIntegral

--return numbers in square as a list. squares are numbered  from left to right and top to bottom
--e.g. :
--[0,1,2]
--[3,4,5]
--[6,7,8]
selectSquare :: [[Int]] -> Int -> [Int]
selectSquare xss i =  undefined

-- HA 3.1b)
isValidSubsection :: [Int] -> Bool
isValidSubsection = undefined

isValidSudoku :: [[Int]] -> Bool
isValidSudoku xss = undefined 

-- HA 3.1c)
setCell :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
setCell xss (j, k) x = undefined 

-- HA 3.1d)
{-WETT-}
solveSudoku :: [[Int]] -> [[Int]]
solveSudoku xss = undefined
{-TTEW-}

hardSudoku :: [[Int]]
hardSudoku = [[8,0,0,0,0,0,0,0,0],
              [0,0,3,6,0,0,0,0,0],
              [0,7,0,0,9,0,2,0,0],
              [0,5,0,0,0,7,0,0,0],
              [0,0,0,0,4,5,7,0,0],
              [0,0,0,1,0,0,0,3,0],
              [0,0,1,0,0,0,0,6,8],
              [0,0,8,5,0,0,0,1,0],
              [0,9,0,0,0,0,4,0,0]]

-- Utility method to show a sudoku
-- show sudoku with
-- >>> putStr (showSudoku sudoku)
showSudoku :: [[Int]] -> String
showSudoku xss = unlines $ intercalate [showDivider] $ chunksOf squareSize $ map showRow xss
  where
    size = length xss
    squareSize = intRoot size 
    numberSize = size `div` 10 + 1

    showRowSection xs = unwords $ map (printf ("%0" ++ show numberSize ++ "d")) xs
    showRow xs = intercalate "|" $ map showRowSection $ chunksOf squareSize xs
    showDivider = intercalate "+" $ replicate squareSize $ replicate ((numberSize + 1) * squareSize - 1) '-'

    chunksOf :: Int -> [e] -> [[e]]
    chunksOf i [] = []
    chunksOf i ls = take i ls : chunksOf i (drop i ls)

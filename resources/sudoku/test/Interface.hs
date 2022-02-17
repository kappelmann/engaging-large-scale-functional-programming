module Interface where

import qualified Exercise03 as E

-- HA 3.2a
selectRow :: [[Int]] -> Int -> [Int]
selectRow = E.selectRow

-- HA 3.2b
selectColumn :: [[Int]] -> Int -> [Int]
selectColumn = E.selectColumn

-- HA 3.2c
selectSquare :: [[Int]] -> Int -> [Int]
selectSquare = E.selectSquare

-- HA 3.2d
isValidSudoku :: [[Int]] -> Bool
isValidSudoku = E.isValidSudoku

-- HA 3.2e
setCell :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
setCell = E.setCell

-- HA 3.2f
solveSudoku :: [[Int]] -> [[Int]]
solveSudoku = E.solveSudoku


module Exercise03 where

import Text.Printf (printf)
import Data.List (intercalate)

-- HA 3.1a) i
selectRow :: [[Int]] -> Int -> [Int]
selectRow xss i = xss !! i

-- HA 3.1a) ii
selectColumn :: [[Int]] -> Int -> [Int]
selectColumn xss i = [xs !! i | xs <- xss]

-- HA 3.1a) iii
intRoot :: Int -> Int
intRoot = floor . sqrt . fromIntegral

-- return numbers in square as a list. squares are numbered  from left to right and top to bottom
-- e.g. :
-- [0,1,2]
-- [3,4,5]
-- [6,7,8]
selectSquare :: [[Int]] -> Int -> [Int]
selectSquare xss i = concat [selectColumns i xs | xs <- selectRows i]
  where
    squareSize = intRoot $ length xss
    selectRows i = take squareSize $ drop ((i `div` squareSize) * squareSize) xss
    selectColumns i xs = take squareSize $ drop ((i `mod` squareSize) * squareSize) xs


-- H3.1b
isValidSubsection :: [Int] -> Bool
isValidSubsection [] = True
isValidSubsection (x:xs) 
  | x == 0 || (x `notElem` xs) = isValidSubsection xs
  | otherwise = False

-- The check that the numbers are between 0 and length of the sudoku
-- is not strictly necessary but is used in the tests.
isValidSudoku :: [[Int]] -> Bool
isValidSudoku xss = and $ [x >= 0 && x <= length xss | x <- concat xss] ++ [
    isValidSubsection (selectSquare xss i) &&
    isValidSubsection (selectRow xss i) &&
    isValidSubsection (selectColumn xss i) | i <- [0..(length xss-1)]
  ] 

-- HA 3.1c)
setCell :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
setCell xss (j, k) x = setAt j (setAt k x (xss !! j)) xss 
  where
    setAt :: Int -> a -> [a] -> [a]
    setAt i x xs = let (hs, ts) = splitAt i xs in hs ++ x : drop 1 ts

-- HA 3.1d)
findFirstNotNull :: [[a]] -> [a]
findFirstNotNull [] = []
findFirstNotNull ([]:xss) = findFirstNotNull xss
findFirstNotNull (xs:_) = xs

solveSudoku :: [[Int]] -> [[Int]]
solveSudoku xss = solveSudokuAux xss (0, 0)
  where
    solveSudokuAux :: [[Int]] -> (Int, Int) -> [[Int]]
    solveSudokuAux xss (j, k)
      | j >= length xss = if isValidSudoku xss then xss else []
      | k >= length xss = solveSudokuAux xss (j + 1, 0)
      | (xss !! j) !! k /= 0 = solveSudokuAux xss (j, k + 1)
      | otherwise =
          let
            isValid i = isValidSudoku (setCell xss (j, k) i)
            solveRecursive i = solveSudokuAux (setCell xss (j, k) i) (j, k + 1)
            recursiveResults = [solveRecursive i | i <- [1..length xss], isValid i]
          in
            findFirstNotNull recursiveResults

-- show sudoku with
-- >>> putStr (showSudoku sudoku)
showSudoku :: [[Int]] -> String
showSudoku xss = unlines $ intercalate [showDivider] $ chunksOf squareSize $ map showRow xss
  where
    size = length xss
    squareSize = intRoot $ fromIntegral size 
    numberSize = size `div` 10 + 1

    showRowSection xs = unwords $ map (printf ("%0" ++ show numberSize ++ "d")) xs
    showRow xs = intercalate "|" $ map showRowSection $ chunksOf squareSize xs
    showDivider = intercalate "+" $ replicate squareSize $ replicate ((numberSize + 1) * squareSize - 1) '-'

    chunksOf :: Int -> [e] -> [[e]]
    chunksOf i [] = []
    chunksOf i ls = take i ls : chunksOf i (drop i ls)



-- Here is a more elegant (and a bit faster) solution using Zippers
-- For a tutorial on zippers, see http://learnyouahaskell.com/zippers

type Zipper a = ([a], [a])

toZipper :: [a] -> Zipper a
toZipper xs = ([], xs)

fromZipper :: Zipper a -> [a]
fromZipper (xs, ys) = reverse xs ++ ys

nextZipper :: Zipper a -> Zipper a
nextZipper (xs, []) = (xs, [])
nextZipper (xs, y:ys) = (y:xs, ys)

solveSudokuZ :: [[Int]] -> [[Int]]
solveSudokuZ xss = solveSudokuZipper (length xss) $ toZipper xss
  where
    solveSudokuRow :: Int -> Zipper [Int] -> Zipper Int -> [[Int]]
    solveSudokuRow n (xss, yss) row@(xs, []) = solveSudokuZipper n $ nextZipper (xss, fromZipper row : yss)
    solveSudokuRow n (xss, yss) (xs, 0:ys) =
        let
          isValid i = isValidSudoku $ fromZipper (xss, fromZipper (i:xs, ys) : yss)
          results = [solveSudokuRow n (xss, yss) (i:xs, ys) | i <- [1..n], isValid i]
        in
          findFirstNotNull results
    solveSudokuRow n sudokuZ rowZ = solveSudokuRow n sudokuZ (nextZipper rowZ)
    
    solveSudokuZipper :: Int -> Zipper [Int] -> [[Int]]
    solveSudokuZipper n sdku@(xss, [])
      | isValidSudoku $ fromZipper sdku = fromZipper sdku
      | otherwise = []
    solveSudokuZipper n (xss, ys:yss) = solveSudokuRow n (xss, yss) (toZipper ys)



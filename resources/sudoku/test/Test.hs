{-# LANGUAGE CPP #-}

module Test where

import qualified Interface as Sub
import qualified Solution as Sol
import qualified Sudokus

import Data.List
import System.Environment (setEnv, getEnv)

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Success, Failure)
import Test.QuickCheck.Property as QCP
import Test.QuickCheck.Assertions
import Test.Tasty.HUnit as HU

prop_selectRowRef (Sudokus.GenDenseSudoku s) =
  QC.forAllShow (choose (0, length s - 1)) (\n -> "selectRow _ " ++ show n) $ \n ->
    Sub.selectRow s n ?== Sol.selectRow s n
    
prop_selectColumnRef (Sudokus.GenDenseSudoku s) =
  QC.forAllShow (choose (0, length s - 1)) (\n -> "selectColumn _ " ++ show n) $ \n ->
    Sub.selectColumn s n ?== Sol.selectColumn s n

prop_selectSquareRef (Sudokus.GenDenseSudoku s) =
  QC.forAllShow (choose (0, length s - 1)) (\n -> "selectSquare _ " ++ show n) $ \n ->
    Sub.selectSquare s n ?== Sol.selectSquare s n

prop_setCellRef (Sudokus.GenDenseSudoku s) =
  QC.forAllShow (Sudokus.chooseCell (1, length s)) (\(c, v) -> "setCell _ " ++ show c ++ " " ++ show v) $ \(c, v) ->
    Sub.setCell s c v ?== Sol.setCell s c v 

prop_isValidSudokuRef (Sudokus.GenDenseSudoku s) =
  classify (Sol.isValidSudoku s) "valid sudoku" $
    Sub.isValidSudoku s ?== Sol.isValidSudoku s

hasEmpty :: [[Int]] -> Bool
hasEmpty = elem 0 . concat

containsSudoku :: [[Int]] -> [[Int]] -> Bool
containsSudoku sol base = aux (concat sol) (concat base)
  where
    aux [] [] = True
    aux (s:ss) (b:bs) = (b == s || b == 0) && aux ss bs
    aux _ _ = False 

doesSolveSudoku :: [[Int]] -> [[Int]] -> Bool
doesSolveSudoku sol base =
  Sol.isValidSudoku sol && not (hasEmpty sol) && containsSudoku sol base

unit_solvableSudoku =
  assertBool ("solveSudoku didn't solve:\n" ++ Sudokus.showSudoku Sudokus.hard)
    (doesSolveSudoku (Sub.solveSudoku Sudokus.hard) Sudokus.hard)

prop_solvableSudokuRef (Sudokus.GenSparseSudoku s) =
  not (null $ Sol.solveSudoku s) QC.==>
    classify (Sol.isValidSudoku s) "valid sudoku" $ 
      let
        subSudoku = Sub.solveSudoku s
      in
        counterexample ("the sudoku you returned does not solve the original sudoku. your sudoku:\n" ++ Sudokus.showSudoku subSudoku) $
          doesSolveSudoku subSudoku s 

prop_solvableDenseSudokuRef (Sudokus.GenDenseSudoku s) =
  not (null $ Sol.solveSudoku s) QC.==>
    classify (Sol.isValidSudoku s) "valid sudoku" $ 
      let
        subSudoku = Sub.solveSudoku s
      in
        counterexample ("the sudoku you returned does not solve the original sudoku. your sudoku:\n" ++ Sudokus.showSudoku subSudoku) $
          doesSolveSudoku subSudoku s 

prop_unsolvableSudokuRef (Sudokus.GenDenseSudoku s) =
  null (Sol.solveSudoku s) QC.==>
    classify (Sol.isValidSudoku s) "valid sudoku" $ 
      let
        subSudoku = Sub.solveSudoku s
      in
        counterexample ("expected empty list since there is no solution but got:\n" ++ Sudokus.showSudoku subSudoku) $
          null subSudoku

tests_HW_3_2 = localOption (QuickCheckTests 1000) $ localOption (QuickCheckMaxSize 4) $ testGroup "Tests Homework 3.2" [
  QC.testProperty "selectRow equals sample solution?" prop_selectRowRef,
  QC.testProperty "selectColumn equals sample solution?" prop_selectColumnRef,
  QC.testProperty "selectSquare equals sample solution?" prop_selectSquareRef,
  QC.testProperty "setCell equals sample solution?" prop_setCellRef,
  QC.testProperty "isValidSudoku equals sample solution?" prop_isValidSudokuRef,
  localOption (mkTimeout (10 * 10^6)) $ HU.testCase "solveSudoku solves hard sudoku?" unit_solvableSudoku,
  localOption (mkTimeout (30 * 10^6)) $ localOption (QuickCheckMaxSize 3) $ QC.testProperty "solveSudoku solves sudoku?" prop_solvableSudokuRef,
  localOption (mkTimeout (30 * 10^6)) $ localOption (QuickCheckMaxSize 3) $ QC.testProperty "solveSudoku solves dense sudoku?" prop_solvableDenseSudokuRef,
  localOption (QuickCheckMaxSize 3) $ localOption (QuickCheckMaxRatio 200) $ QC.testProperty "solveSudoku detects unsolvable sudoku?" prop_unsolvableSudokuRef
                                              ]

-- Final tests wrap up and main
tests = testGroup "Tests Homework 3" [tests_HW_3_2]

#ifdef LOCAL
testRunner = defaultMain
#else
testRunner = defaultMainWithIngredients [antXMLRunner]
#endif

main :: IO ()
main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOptions tests
  where
    resultsPath = "test-reports/results.xml"
    -- by default, run for 1 second
    localOptions = localOption (mkTimeout (2 * 10^6)) . localOption (QuickCheckTests 250) . localOption (QuickCheckShowReplay False)

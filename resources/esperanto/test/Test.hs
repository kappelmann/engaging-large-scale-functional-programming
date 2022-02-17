{-# LANGUAGE CPP #-}
module Test where

import qualified Interface as Sub
import qualified Solution as Sol

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Property as QCP
import Test.QuickCheck.Assertions
import Test.Tasty.HUnit as HU
import Data.List
import Control.Monad

import System.Environment (setEnv)

funCounterex s as = counterexample $ "called: " ++ s ++ " " ++ unwords (map show as)

{-H2-}

-- QuickChecks

qcNoExtraSpaces :: QC.Property
qcNoExtraSpaces = QC.forAll (QC.choose (0, 10^6 - 1)) f
  where f n = counterexample ("\n\"" ++ res ++ "\"") (not ("  " `isInfixOf` res || " " `isPrefixOf` res || " " `isSuffixOf` res))
          where res = Sub.numberToEo n

digits = ["unu", "du", "tri", "kvar", "kvin", "ses", "sep", "ok", "nau"]
mods = ["dek", "cent", "mil"]
allowedWords = digits ++ mods ++ liftM2 (++) digits mods

qcOnlyAllowedWords :: QC.Property
qcOnlyAllowedWords = QC.forAll (QC.choose (1, 10^6 - 1)) f
  where f n = counterexample ("Unknown word: \"" ++ head forbidden ++ "\"") (null forbidden)
          where forbidden = nub (words (Sub.numberToEo n)) \\ allowedWords

prop_qcEqSol2 = QC.forAll (QC.choose (10^5 + 1, 10^6 - 1)) $ \n -> funCounterex "numberToEo" [n] $ Sol.numberToEo n ==? Sub.numberToEo n

prop_qcEqSol2All ns =
  case xs of
    [] -> counterexample "" True
    ((n, exp, act) : _) -> counterexample ("numberToEo " ++ show n ++ "\n  expected: " ++ show exp ++ "\n  actual: " ++ show act) False
  where xs = [(n, exp, act) | n <- ns, let act = Sub.numberToEo n, let exp = Sol.numberToEo n, exp /= act]

props_H1_2 :: TestTree
props_H1_2 = testGroup "Tests H1.2" [
    QC.testProperty "equals sample solution for small numbers?" $ prop_qcEqSol2All [0..10^5],
    QC.testProperty "equals sample solution for big numbers?" (QC.withMaxSuccess 1000 prop_qcEqSol2),
    QC.testProperty "Testing for extra space characters" (QC.withMaxSuccess 1000 qcNoExtraSpaces),
    QC.testProperty "Testing for unknown words" (QC.withMaxSuccess 1000 qcOnlyAllowedWords)
  ]

-- Final tests wrap up and main
tests = testGroup "Tests Homework 1" [props_H1_2]


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
  testRunner $ localOption timeoutOption $ localOption (QuickCheckTests 250) tests
  where
    resultsPath = "test-reports/results.xml"
    -- by default, run for 1 second
    timeoutOption = mkTimeout (2 * 10^6)

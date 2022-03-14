{-# LANGUAGE CPP #-}
module Test where

import qualified Interface as Sub
import qualified Solution as Sol

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Assertions
import Test.Tasty.HUnit

import Control.Monad
import Data.List(sort,delete,nub)

import qualified Mock.System.IO.Internal as MI
import qualified Mock.System.IO.RealWorld as MR

import System.Environment (setEnv)

-- SmallChecks

scProps :: TestTree
scProps = localOption (SC.SmallCheckDepth 3) $ testGroup "Checked by SmallCheck" [
  ]

-- QuickChecks
-- 12.2
getAverage s i j ps =
  let filtered = [p | (n,t,p) <- ps, n == s, t >= i, t <= j]
  in if null filtered then 0 else sum filtered `div` length filtered

genStock = elements ["BB","BBY","AMC","GME"]
genTicker = do
  n <- genStock
  t <- choose (0,100) :: Gen Int
  p <- choose (0,100) :: Gen Int
  return (n,t,p)

genTickers = listOf genTicker

-- QuickChecks
--
prop_stocks' s i j ps = MR.evalIO wrappedIo MR.emptyWorld
  where
    wrappedIo = wrapConsoleProp $ (MR.setUser user >> Sub.main >> MR.runUser)
    readAll = do b <- MI.hIsEOF MI.stdout
                 if b then return [] else liftM2 (:) (MI.hGetLine MI.stdout) readAll
    user = do MI.hPutStrLn MI.stdin (unwords [s, show (i, j)])
              mapM_ (MI.hPutStrLn MI.stdin) $ map (\(n,t,p) -> n ++ "," ++ show t ++ "," ++ show p) ps
              MI.hPutStrLn MI.stdin "quit" >> MR.wait
              output <- fmap unlines readAll
              let expected = getAverage s i j ps
              when (read output /= expected)
                (fail ("### Wrong result. \n### Expected output: " ++ show expected ++ "\n### Actual output: " ++ output))

prop_stocks = QC.forAll genStock $ \ s -> QC.forAll (choose (0,20) :: Gen Int) $ \ i -> QC.forAll (choose (15,50) :: Gen Int) $ \ j -> QC.forAll genTickers $ \ ps -> prop_stocks' s i j ps 
wrapConsoleProp io =
  do hook <- MR.hookConsole
     r <- MR.tryIO io
     consoleDump <- fmap (\d -> "\n=== CONSOLE DUMP ===\n" ++ d ++ "\n=== END CONSOLE DUMP ===\n") (MR.showConsoleHook hook)
     let p' = case r of
                Right p -> counterexample consoleDump p 
                Left e -> let msg = if MI.isUserError e then 
                                      "### " ++ MI.ioeGetErrorString e ++ "\n"
                                    else
                                      "\n### IO Exception thrown: " ++ show e ++ "\n"
                          in counterexample msg $ counterexample consoleDump (property False)
     return p'


stonksProps :: TestTree
stonksProps = testGroup "Stonks" [
    localOption (QC.QuickCheckMaxSize 100) $ QC.testProperty "testing stonks main against sample solution" prop_stocks
  ]

tests :: TestTree
tests = testGroup "Tests" [stonksProps]

main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOption timeoutOption tests
  where
    resultsPath = "test-reports/results.xml"
#ifdef PROD 
    -- on the server (production mode), run tests with xml output
    testRunner = defaultMainWithIngredients [antXMLRunner]
#else
    -- locally, run tests with terminal output
    testRunner = defaultMain
#endif    
    -- by default, run for 1 second
    timeoutOption = mkTimeout (10 * 10^6)

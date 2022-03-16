{-# LANGUAGE CPP #-}
module Test where

import qualified Interface as Sub
import qualified Solution as Sol

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.Tasty.QuickCheck as QC

import Control.Monad

import qualified Mock.System.IO.Internal as Mock

import System.Environment (setEnv)

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

-- We show the console dump if the test fails and fail the test if an error occurs
wrapConsoleProp io = do
  hook <- Mock.hookConsole
  r <- Mock.tryIO io
  consoleDump <- fmap (\d -> "\n=== CONSOLE DUMP ===\n" ++ d ++ "\n=== END CONSOLE DUMP ===\n") (Mock.showConsoleHook hook)
  let p' = case r of
             Right p -> counterexample consoleDump p 
             Left e -> counterexample (showError e) $ counterexample consoleDump (property False)
  return p'
  where
    showError e = if Mock.isUserError e
                    then "### " ++ Mock.ioeGetErrorString e ++ "\n"
                    else "\n### IO Exception thrown: " ++ show e ++ "\n"


prop_stocks' s i j ps = Mock.evalIO wrappedIo Mock.emptyWorld
  where
    wrappedIo = wrapConsoleProp $ Mock.setUser user >> Sub.main >> Mock.runUser

    getContentS = do
      b <- Mock.hIsEOF Mock.stdout
      if b then return [] else liftM2 (:) (Mock.hGetLine Mock.stdout) getContentS

    user = do
      Mock.hPutStrLn Mock.stdin (unwords [s, show (i, j)])
      mapM_ (Mock.hPutStrLn Mock.stdin) $ map (\(n,t,p) -> n ++ "," ++ show t ++ "," ++ show p) ps
      Mock.hPutStrLn Mock.stdin "quit"
      Mock.wait -- this transfers control to the submission code
      output <- unlines <$> getContentS
      let expected = getAverage s i j ps
      when (read output /= expected)
        (fail ("### Wrong result. \n### Expected output: " ++ show expected ++ "\n### Actual output: " ++ output))

prop_stocks =
  QC.forAll genStock $ \s ->
    QC.forAll (choose (0,20) :: Gen Int) $ \i ->
      QC.forAll (choose (15,50) :: Gen Int) $ \j ->
        QC.forAll genTickers $ \ps ->
          prop_stocks' s i j ps 


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

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

import System.Environment (setEnv)

-- CYP setup
import Text.PrettyPrint (render)
import Data.Tagged (Tagged (Tagged))
import qualified Test.Tasty.Providers as TP
import qualified Test.Tasty.Options as TO
import Data.Bifunctor
import Data.List
import qualified Data.Bits as B
import Data.Typeable (Typeable)
import System.FilePath ((</>))
import qualified Test.Info2.Cyp as Cyp
import Text.Printf

-- CYP stuff

newtype SubmissionPath = SubmissionPath FilePath

instance TO.IsOption SubmissionPath where
  defaultValue = undefined -- laziness ftw
  parseValue = undefined
  optionName = undefined
  optionHelp = undefined

data CypTest = CypTest {
  theory :: FilePath,
  proof :: FilePath
} deriving Typeable

instance TP.IsTest CypTest where
  testOptions = Tagged []
  run opts t _ = either (TP.testFailed . render) (const $ TP.testPassed "+++ Q.E.D. ❤️") <$> Cyp.proofFile thy prf
    where (SubmissionPath submissionPath) = TO.lookupOption opts
          thy = thyPath </> theory t
          prf = submissionPath </> proof t
          thyPath = "test/cthy"

cypProof :: TestName -> CypTest -> TestTree
cypProof = TP.singleTest

-- CYP tests

cypTests :: TestTree
cypTests = testGroup "CYP proofs" [
    cypProof "HA 5.2" $ CypTest { theory = "h52.cthy", proof = "h52.cprf" },
    cypProof "HA 5.3" $ CypTest { theory = "h53.cthy", proof = "h53.cprf" }
  ]

tests :: TestTree
tests = testGroup "Tests" [cypTests]

main :: IO ()
main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOption (SubmissionPath submissionPath) $ localOption timeoutOption tests
  where
    resultsPath = "test-reports/results.xml"
    -- by default, run for 1 second
    timeoutOption = mkTimeout (1 * 10^6)
#ifdef LOCAL
    testRunner = defaultMain
#else
    testRunner = defaultMainWithIngredients [antXMLRunner]
#endif
-- CYP setup; also set the flags in the cabal file!
#ifdef TEST
    submissionPath = "assignment/src"
#endif
#ifdef TESTTEMPLATE
    submissionPath = "template/src"
#endif
#ifdef TESTSOLUTION
    submissionPath = "solution/src"
#endif

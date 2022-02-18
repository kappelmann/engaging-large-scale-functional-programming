{-# LANGUAGE CPP #-}
module Test where

import qualified Interface as Sub
import qualified Solution as Sol

import qualified Mock.System.IO.Internal as MI
import qualified Mock.System.IO.RealWorld as MR

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QCG
import Test.QuickCheck.Assertions
import Test.Tasty.HUnit

import Data.Char (isSpace)
import Data.List
import System.Environment (setEnv)
import Control.Monad

alph = ['F','+','-','X','Y','*','~'] 

instance Arbitrary Sol.Rule where
  arbitrary = (Sol.:->:) <$> elements alph <*> listOf1 (elements alph)

  shrink (a Sol.:->: as) = map (a Sol.:->:) $ shrink as

instance Arbitrary Sol.LSystem where
  arbitrary = Sol.LSystem <$> listOf1 (elements alph) <*> arbitrary

  shrink (Sol.LSystem s rs) = map (Sol.LSystem s) $ shrink rs

data ValidCommand = Print | Clear | Start String | Rule Sol.Rule

instance Arbitrary ValidCommand where
  arbitrary = do
    frequency [ (2, return Print), (1, return Clear), (3, genStart), (6, Rule <$> arbitrary) ]
      where
        genStart = Start <$> listOf1 (elements alph) 

  shrink (Rule rl) = Rule <$> shrink rl
  shrink _ = []

instance Show ValidCommand where
  show Print = "print"
  show Clear = "clear"
  show (Start c) = "start " ++ c
  show (Rule (a Sol.:->: as)) = "rule " ++ [a] ++ " -> " ++ as

data InvalidCommand = InvalidCommand String | InvalidRule String

instance Arbitrary InvalidCommand where
  arbitrary = do
    frequency [(1, InvalidCommand <$> genInvalidCmd), (1, InvalidRule <$> genInvalidRule)]
    where
      genInvalidCmd = do
        head . filter (\x -> not $ any (`isPrefixOf` x) ["print", "clear", "start", "rule"]) . map (take 20) <$> infiniteListOf (listOf1 $ elements (['a'..'z'] ++ " ->"))

      genInvalidRule = do
        c <- elements "FXY"
        wrongArrow <- elements ["<-", ">>", ">", ">-", "-", "-->", "->-"]
        rhs <- listOf1 $ elements alph
        rl <- frequency [(1, return $ unwords [[c], wrongArrow, rhs]), (1, return $ unwords [[c, c], "->", rhs])]
        return $ "rule " ++ rl

instance Show InvalidCommand where
  show (InvalidCommand c) = c
  show (InvalidRule c) = c

data Command = Valid ValidCommand | Invalid InvalidCommand

instance Show Command where
  show (Valid c) = show c
  show (Invalid c) = show c

instance Arbitrary Command where
  arbitrary = frequency [(7, Valid <$> arbitrary), (1, Invalid <$> arbitrary)]

  shrink (Valid cmd) = Valid <$> shrink cmd
  shrink _ = []

newtype Commands = Commands [Command]

instance Show Commands where
  show (Commands cmds) = unlines $ map show cmds

instance Arbitrary Commands where
  arbitrary = Commands <$> listOf1 arbitrary
  shrink (Commands cmds) = Commands <$> shrink cmds
    

toSolRule :: Sub.Rule -> Sol.Rule
toSolRule (x Sub.:->: y) = x Sol.:->: y
toSubRule :: Sol.Rule -> Sub.Rule
toSubRule (x Sol.:->: y) = x Sub.:->: y

toSubLSys :: Sol.LSystem -> Sub.LSystem
toSubLSys Sol.LSystem{Sol.start = s, Sol.rules = rs} =
  Sub.LSystem {Sub.start = s, Sub.rules = map toSubRule rs}

lsystemsTest :: [Sol.LSystem]
lsystemsTest =
  [Sol.LSystem "+++F" ['F' Sol.:->: "F+++F---F---F+++F"],
   Sol.LSystem "FX" ['X' Sol.:->: "X+++YF+++", 'Y' Sol.:->: "---FX---Y"],
   Sol.LSystem "F" ['F' Sol.:->: "F+++F---F---F+++F"],
   Sol.LSystem "F++++G++++G" ['F' Sol.:->: "F++++G----F----G++++F", 'G' Sol.:->: "GG"],
   Sol.LSystem "F" []
  ]

prop_findRule :: ([Sub.Rule] -> Char -> Sub.Rule) -> ([Sol.Rule] -> Char -> Sol.Rule) -> Sol.LSystem -> QC.Property
prop_findRule sub sol lsys =
  QC.forAll (elements alph)
    (\c -> toSolRule (sub (map toSubRule (Sol.rules lsys)) c) == sol (Sol.rules lsys) c)

prop_expandLsystem :: (Sub.LSystem -> Integer -> String) -> (Sol.LSystem -> Integer -> String) -> Sol.LSystem -> QC.Property
prop_expandLsystem sub sol lsys = QC.forAll (QCG.choose (0,3)) (\n -> sub (toSubLSys lsys) n == sol lsys n)

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

prop_update (Commands cmds) = MR.evalIO wrappedIO MR.emptyWorld
  where
    wrappedIO = wrapConsoleProp (MR.setUser user >> Sub.update (Sub.LSystem "" []) >> MR.runUser)
    readAll = do b <- MI.hIsEOF MI.stdout
                 if b then return [] else liftM2 (:) (MI.hGetLine MI.stdout) readAll
    user = do
      subOut <- mapM ((\c -> MI.hPutStrLn MI.stdin c >> MR.wait >> unlines <$> readAll) . show) cmds
      case compareResults (Sol.LSystem "" []) (zip cmds $ subOut ++ repeat "") of
        Nothing -> return ()
        Just (subOut, solOut) -> wrongRes subOut solOut

    wrongRes output expected = fail ("### Wrong console output\n### Expected output: " ++ expected ++ "\n### Actual output:\n" ++ output)

    compareResults ls [] = Nothing 
    compareResults ls ((cmd, subOut) : us) =
      case Sol.parseApplyCmd (show cmd) ls of
        Right ls' -> compareResults ls' us
        Left solOut -> if trim subOut == trim solOut
                          then compareResults ls us
                          else Just (subOut, solOut)

    trimL = dropWhile isSpace
    trimR = reverse . trimL . reverse
    trim = trimL . trimR


-- Tests
qcProps :: TestTree
qcProps = testGroup "Checked by QuickCheck" [
    QC.testProperty "Testing findRule against sample solution" $ prop_findRule Sub.findRule Sol.findRule,
    QC.testProperty "Testing expandLSystem against sample solution" $ prop_expandLsystem Sub.expandLSystem Sol.expandLSystem,
    localOption (QuickCheckTests 1000) $ QC.testProperty "Testing update against sample solution" prop_update
  ]

-- UnitTests
exampleTests :: TestTree
exampleTests = testGroup "Tests with example L-systems" [
    QC.testProperty "Testing findRule" $ QC.forAll (elements lsystemsTest) $ \lsys -> prop_findRule Sub.findRule Sol.findRule lsys,
    QC.testProperty "Testing expandLSystem" $ QC.forAll (elements lsystemsTest) $ \lsys -> prop_expandLsystem Sub.expandLSystem Sol.expandLSystem lsys
  ]

-- Final tests wrap up and main
tests :: TestTree
tests = testGroup "Tests" [qcProps,exampleTests]

main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOption timeoutOption tests
  where
    resultsPath = "test-reports/results.xml"
    timeoutOption = mkTimeout (5 * 10^6)
#ifdef PROD
    testRunner = defaultMainWithIngredients [antXMLRunner]
#else
    testRunner = defaultMain
#endif


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

import System.Environment (setEnv)

class Show a => VAEq a where
  (~~=) :: a -> a -> Bool
  (~~=?) :: a -> a -> Result
  (~~=?) a b = binAsrt s (a ~~= b) where
    s = "\nexpected value: " ++ show b
      ++ "\nactual value: " ++ show a

vaComp :: (Num a, Ord a) => a -> a -> a -> Bool
vaComp epsilon a b = b - epsilon <= a && a <= b + epsilon

instance VAEq Integer where
  (~~=) = vaComp 1

instance VAEq Int where
  (~~=) = vaComp 1

instance VAEq Double where
  (~~=) a b = vaComp (1 / 16) a b || isNaN a && isNaN b

instance (VAEq a) => VAEq [a] where
  (~~=) a b = length a == length b && all (uncurry (~~=)) (zip a b)
  (~~=?) a b = binAsrt s (a ~~= b) where
    (x, y, i) = head $ filter (\(a, b, i) -> not $ a ~~= b) (zip3 a b [0..])
    s = if length a /= length b
      then "expected length: " ++ show (length b)
        ++ "\nactual length: " ++ show (length a)
      else "at index " ++ show i
        ++ "\n  expected value: " ++ show y
        ++ "\n  actual value: " ++ show x

testValues domain sigSub sigSol =
  QC.forAll (choose domain) (\x ->
    QC.counterexample ("at input " ++ show x) $ sigSub x ~~=? sigSol x)

prop_sinPeriod = testValues (0, 1) Sub.sinPeriod Sol.sinPeriod

prop_sqwPeriod =
  QC.forAll (choose (0, 1)) (\x ->
    x /= 0.5 QC.==>
    QC.counterexample ("at input " ++ show x) $ Sub.sqwPeriod x ~~=? Sol.sqwPeriod x)

prop_sqwPeriodMid = let
    y = Sub.sqwPeriod 0.5
    s = "`sqwPeriod 0.5` is not in range [-1, 1]. Actual value: " ++ show y
  in binAsrt s (-1 <= y && y <= 1)

prop_sawPeriod = testValues (0, 1) Sub.sawPeriod Sol.sawPeriod

prop_triPeriod = testValues (0, 1) Sub.triPeriod Sol.triPeriod

genUnitIv = choose (0.0, 1.0)

genADSR = liftM4 (\a d s r -> (a / 4, d / 4, s, r / 4)) genUnitIv genUnitIv genUnitIv genUnitIv 
genSmallADSR = (\(a, d, s, r) -> (a / 250, d / 250, s, r / 250)) <$> genADSR
genSemitone = choose (-48::Integer, 39) -- A0 to C8
gemDuration = choose (0.0035, 0.0050)
prop_adsrEnvelope = QC.forAll gen test where
  test (adsrParams, duration, t) =
    QC.counterexample ("ADSR parameters: " ++ show adsrParams
    ++ "\nduration: " ++ show duration
    ++ "\nposition: " ++ show t) $
      Sub.adsr adsrParams duration (const 1) t ~~=? Sol.adsr adsrParams duration (const 1) t
  gen = liftM2 (,) genSmallADSR gemDuration >>= (\(adsr, duration) -> (\t -> (adsr, duration, t)) <$> choose (0, duration))

prop_adsrScale = QC.forAll gen test where
  test (adsrParams, duration, t) =
    QC.counterexample ("ADSR parameters: " ++ show adsrParams
    ++ "\nduration: " ++ show duration
    ++ "\nposition: " ++ show t) $
      Sub.adsr adsrParams duration Sub.sinPeriod t ~~=? Sol.adsr adsrParams duration Sub.sinPeriod t
  gen = liftM2 (,) genSmallADSR gemDuration >>= (\(adsr, duration) -> (\t -> (adsr, duration, t)) <$> choose (0, duration))

prop_osc = QC.forAll gen test where
  test (adsrParams, semitone, duration, t) =
    QC.counterexample ("ADSR parameters: " ++ show adsrParams
      ++ "\nsemitone: " ++ show semitone
      ++ "\nduration: " ++ show duration
      ++ "\nposition: " ++ show t) $
      Sub.osc Sol.triPeriod adsrParams semitone duration t ~~=? Sol.osc Sol.triPeriod adsrParams semitone duration t
  gen = liftM3 (,,) genSmallADSR genSemitone gemDuration >>=
    (\(adsr, tone, duration) -> (\t -> (adsr, tone, duration, t)) <$> choose (0, duration))

genSignal = QC.listOf1 $ (QC.resize 15000 $ QC.choose (-1.0, 1.0::Double))
genSignals = QC.listOf1 $ QC.resize 10 genSignal

prop_mixLength = QC.forAll genSignals
  (\signals -> length (Sub.mix signals) ~~=? foldl (\acc x -> max acc $ length x) 0 signals)

prop_mixValue = QC.forAll genSignals
  (\signals -> Sub.mix signals ~~=? Sol.mix signals)

-- Execute 250 tests for every QuickCheck test in this test group
tests :: TestTree
tests = localOption (QuickCheckTests 250) $ testGroup "Tests" [
    -- Add a QuickCheck test to tasty
    QC.testProperty "Test sinPeriod" prop_sinPeriod,
    QC.testProperty "Test sqwPeriod 1" prop_sqwPeriod,
    QC.testProperty "Test sqwPeriod 2" prop_sqwPeriodMid,
    QC.testProperty "Test sawPeriod" prop_sawPeriod,
    QC.testProperty "Test triPeriod" prop_triPeriod,
    QC.testProperty "Test adsr envelope" prop_adsrEnvelope,
    QC.testProperty "Test adsr scale" prop_adsrScale,
    QC.testProperty "Test osc" prop_osc,
    QC.testProperty "Test mix length" prop_mixLength,
    QC.testProperty "Test mix values" prop_mixValue
  ]

main :: IO ()
main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOption timeoutOption tests
  where
    resultsPath = "test-reports/results.xml"
    -- run tests with terminal output
    -- by default, run for 1 second
    timeoutOption = mkTimeout (1 * 10^6)
#ifdef LOCAL
    testRunner = defaultMain
#else
    testRunner = defaultMainWithIngredients [antXMLRunner]
#endif


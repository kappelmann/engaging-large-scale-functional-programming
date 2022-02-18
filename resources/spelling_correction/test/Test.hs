{-# LANGUAGE CPP #-}

module Test where

import qualified Interface as Ex4
import qualified Solution as SEx4

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Assertions
import Test.Tasty.HUnit

import Control.Monad
import Control.Spoon (spoon, teaspoon)

import System.Environment (setEnv)
import Data.List (group, sort, sortOn, nub)
import Data.Function (on)

nonNegative = fmap QC.getNonNegative arbitrary
positive = fmap QC.getPositive arbitrary

isClose :: (Show i, Num i, Ord i) => i -> i -> i -> QC.Property
isClose eps user ref = counterexample msg $ abs (ref - user) < eps
  where msg =
          "### Expected output:\n" ++ show ref ++ "\n" ++
          case teaspoon user of
            Just res -> "### Actual output:\n" ++ show res ++ "\n"
            Nothing -> "### Exception thrown"

genPolynomial :: Gen [(Rational,Integer)]
genPolynomial =
  do cs <- listOf (fmap getNonZero arbitrary)
     is <- listOf nonNegative
     return . zip cs . map head . group . sort $ is

{-H4.1.1-}
simpleString = elements ["a", "b", "c", "ab", "ba", "bc", "cb", "ac", "ca", "abc"]
simpleStringList = frequency [ (1, return []), (9, listOf1 simpleString) ]

prop_isMultiSetRef = QC.forAll (arbitrary :: Gen [(Char, Int)]) $ \mx -> Ex4.isMultiSet mx ==? SEx4.isMultiSet mx

genMultiSet = do
    ss <- simpleStringList 
    ms <- infiniteListOf positive
    return $ zip (nub ss) ms

genMultiSetList = fmap (SEx4.toList) genMultiSet >>= QC.shuffle

prop_toListRef = QC.forAll genMultiSet $ \ms -> ((==?) `on` sort) (Ex4.toList ms) (SEx4.toList ms)

prop_toSetRef = QC.forAll genMultiSet $ \ms -> ((==?) `on` sort) (Ex4.toSet ms) (SEx4.toSet ms)

prop_toMultiSetRef = QC.forAll genMultiSetList $ \ms -> ((==?) `on` sortOn fst) (Ex4.toMultiSet ms) (SEx4.toMultiSet ms)

prop_isMultiSet_toMultiSet = QC.forAll genMultiSetList $ \ms -> SEx4.isMultiSet (Ex4.toMultiSet ms)

prop_multiplicityRef = do
    ms <- genMultiSet
    idx <- choose (0, length ms)
    e <- simpleString
    let e' = if idx == length ms then e else fst $ ms !! idx
        in return $ Ex4.multiplicity e' ms ==? SEx4.multiplicity e' ms

prop_dotProductRef =
    QC.forAll genMultiSet $ \ms1 ->
    QC.forAll genMultiSet $ \ms2 ->
        Ex4.dotProduct ms1 ms2 ==? SEx4.dotProduct ms1 ms2

prop_euclideanRef =
    QC.forAll genMultiSet $ \ms ->
        isClose 0.001 (Ex4.euclidean ms) (SEx4.euclidean ms)

prop_cosineRef =
    QC.forAll genMultiSet $ \ms1 ->
    QC.forAll genMultiSet $ \ms2 ->
        not (null ms1 || null ms2) QC.==>
            isClose 0.001 (Ex4.cosine ms1 ms2) (SEx4.cosine ms1 ms2)

genText = fmap unwords (listOf1 simpleString) 

testWords = ["the","of","and","to","in","for","is","on","that","by","this","with",
  "you","it","not","or","be","are","from","at","as","your","all","have","new","more",
  "an","was","we","will","home","can","us","if","page","my","has","free","but","our",
  "one","do","no","time","they","site","he","up","may","what","news","out","use","any",
  "see","only","so","his","when","here","who","web","also","now","help","get","pm",
  "view","am","been","how","were","me","some","its","like","than","find","date","back",
  "top","had","list","name","just","over","year","day","into","two","re","next","used",
  "go","work","last","most","buy","data","make"]

takeFromList :: [a] -> Gen a
takeFromList xs =
    do i <- choose (0, length xs - 1)
       return $ xs !! i

prop_vocabSimilarityRef =
    QC.forAll genText $ \ms1 ->
    QC.forAll genText $ \ms2 ->
        isClose 0.001 (Ex4.vocabSimilarity ms1 ms2) (SEx4.vocabSimilarity ms1 ms2)

prop_editDistanceIdent =
    QC.forAll (takeFromList testWords) $ \s ->
      Ex4.editDistance s s ==? 0

prop_editDistanceRef =
    QC.forAll (takeFromList testWords) $ \s1 ->
    QC.forAll (takeFromList testWords) $ \s2 ->
      Ex4.editDistance s1 s2 ==? SEx4.editDistance s1 s2

prop_spellCorrectRef =
    QC.forAll (takeFromList testWords) $ \s1 ->
      map sort (Ex4.spellCorrect SEx4.frequentWords [s1]) ==? (map sort (SEx4.spellCorrect SEx4.frequentWords [s1]))

tests :: TestTree
tests = testGroup "Tests" [
    testGroup "Timeout 1 second" [
      QC.testProperty "Comparing isMultiSet against sample solution" prop_isMultiSetRef,
      QC.testProperty "Comparing toList against sample solution" prop_toListRef,
      QC.testProperty "Comparing toSet against sample solution" prop_toSetRef,
      QC.testProperty "Comparing toMultiSet against sample solution" prop_toMultiSetRef,
      QC.testProperty "Verifiying that toMultiSet returns a proper multiset" prop_isMultiSet_toMultiSet,
      QC.testProperty "Comparing multiplicity against sample solution" prop_multiplicityRef,
      QC.testProperty "Comparing dotProduct against sample solution" prop_dotProductRef,
      QC.testProperty "Comparing euclidean against sample solution" prop_euclideanRef,
      QC.testProperty "Comparing cosine against sample solution" prop_cosineRef,
      QC.testProperty "Comparing vocabSimilarity against sample solution" prop_vocabSimilarityRef
    ],
    localOption (mkTimeout $ 5 * 10^6) $testGroup "Timeout 5 seconds" [
      QC.testProperty "Verifying that editDistance between identical words is zero" prop_editDistanceIdent,
      QC.testProperty "Comparing editDistance against sample solution" prop_editDistanceRef,
      QC.testProperty "Comparing spellCorrect against sample solution" prop_spellCorrectRef
    ]
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


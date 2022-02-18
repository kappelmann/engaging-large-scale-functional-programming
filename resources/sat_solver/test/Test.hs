{-# LANGUAGE CPP #-}
{-Comment Kevin: Note a particularly nice test suite, but hey, it works!-}

module Test where

import qualified Interface as Ex9
import qualified Solution as SEx9

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Assertions
import Test.Tasty.HUnit

import System.Environment (setEnv)
import Data.List (sort)
import Data.Maybe (isJust)
import Control.DeepSeq

instance NFData (SEx9.Atom) where
  rnf a = ()

instance NFData (SEx9.Literal) where
  rnf l = ()

instance NFData (SEx9.Form) where
  rnf f = ()

instance Ord (SEx9.Atom) where
  compare SEx9.T _ = GT
  compare (SEx9.Var _) SEx9.T = LT
  compare (SEx9.Var n) (SEx9.Var m) = compare m n

instance Ord (SEx9.Literal) where
  compare (SEx9.Pos a) (SEx9.Neg _) = LT
  compare (SEx9.Neg a) (SEx9.Pos _) = GT
  compare (SEx9.Pos a) (SEx9.Pos b) = compare b a
  compare (SEx9.Neg a) (SEx9.Neg b) = compare b a

toExAtom :: SEx9.Atom -> Ex9.Atom
toExAtom SEx9.T = Ex9.T
toExAtom (SEx9.Var n) = Ex9.Var n

toExLiteral :: SEx9.Literal -> Ex9.Literal
toExLiteral (SEx9.Pos a) = Ex9.Pos $ toExAtom a
toExLiteral (SEx9.Neg a) = Ex9.Neg $ toExAtom a

toExForm :: SEx9.Form -> Ex9.Form
toExForm (SEx9.L l) = Ex9.L $ toExLiteral l
toExForm ((SEx9.:&:) f1 f2) = (Ex9.:&:) (toExForm f1) (toExForm f2)
toExForm ((SEx9.:|:) f1 f2) = (Ex9.:|:) (toExForm f1) (toExForm f2)

toExClause :: SEx9.Clause -> Ex9.Clause
toExClause = map toExLiteral

toExConj :: SEx9.ConjForm -> Ex9.ConjForm
toExConj = map toExClause

toSExAtom :: Ex9.Atom -> SEx9.Atom
toSExAtom Ex9.T = SEx9.T
toSExAtom (Ex9.Var n) = SEx9.Var n

toSExLiteral :: Ex9.Literal -> SEx9.Literal
toSExLiteral (Ex9.Pos a) = SEx9.Pos $ toSExAtom a
toSExLiteral (Ex9.Neg a) = SEx9.Neg $ toSExAtom a

toSExForm :: Ex9.Form -> SEx9.Form
toSExForm (Ex9.L l) = SEx9.L $ toSExLiteral l
toSExForm ((Ex9.:&:) f1 f2) = (SEx9.:&:) (toSExForm f1) (toSExForm f2)
toSExForm ((Ex9.:|:) f1 f2) = (SEx9.:|:) (toSExForm f1) (toSExForm f2)

toSExClause :: Ex9.Clause -> SEx9.Clause
toSExClause = map toSExLiteral

toSExConj :: Ex9.ConjForm -> SEx9.ConjForm
toSExConj = map toSExClause

genName :: Int -> Gen SEx9.Name
genName mV = do
  n <- choose (1, mV)
  return $ show n

-- mV: maximum variable index
-- fV: frequency of variables
-- fT: frequency of True
genAtom :: Int -> Int -> Int -> Gen SEx9.Atom
genAtom mV fV fT = do
    v <- genName mV 
    frequency [(fV, return $ SEx9.Var v), (fT, return $ SEx9.T)]

-- fP: frequency of positive literals
-- fN: frequency of negative literals
genLiteral :: Int -> Int -> Int -> Int -> Int -> Gen SEx9.Literal
genLiteral mV fV fT fP fN = do
    a <- genAtom mV fV fT
    frequency [(fP, return $ SEx9.Pos a), (fN, return $ SEx9.Neg a)]

-- lC: maximum size of clauses
genClause :: Int -> Int -> Int -> Int -> Int -> Int -> Gen SEx9.Clause
genClause mV fV fT fP fN lC = resize lC $ listOf (genLiteral mV fV fT fP fN)

-- lC: maximum number of clauses
genConj :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Gen SEx9.ConjForm
genConj mV fV fT fP fN lC l = resize l $ listOf (genClause mV fV fT fP fN lC)

-- same as genClause but generates only non-empty clauses
genClauseNe :: Int -> Int -> Int -> Int -> Int -> Int -> Gen SEx9.Clause
genClauseNe mV fV fT fP fN lC = resize lC $ listOf1 (genLiteral mV fV fT fP fN)

-- same as genConj but generates at least one non-empty clauses 
genConjNe :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Gen SEx9.ConjForm
genConjNe mV fV fT fP fN lC l = resize l $ listOf1 (genClauseNe mV fV fT fP fN lC)

-- n: exponential depth of formula
genForm :: Int -> Int -> Int -> Int -> Int -> Int -> Gen SEx9.Form
genForm mV fV fT fP fN 0 = do 
  l <- genLiteral mV fV fT fP fN
  return $ SEx9.L l
genForm mV fV fT fP fN n | n > 0 = do 
    f1 <- genForm mV fV fT fP fN (n-1)
    f2 <- genForm mV fV fT fP fN (n-1)
    oneof [return $ (SEx9.:&:) f1 f2, return $ (SEx9.:|:) f1 f2]

simpNonTrivial :: SEx9.ConjForm -> Bool
simpNonTrivial f = not $ SEx9.simpConj f `elem` [[], [[]]] 

prop_simpConj mV fV fT fP fN lC l = 
  QC.forAll (genConj mV fV fT fP fN lC l) $ \f ->
  classify (simpNonTrivial f) "non-trivial" $
  let fsub = Ex9.simpConj (toExConj f)
      fsol = SEx9.simpConj f
  in length fsub ==? length fsol .&&. (sort $ toSExConj $ fsub) ==? (sort $ fsol)


prop_simpConjNe mV fV fT fP fN lC l = 
  QC.forAll (genConjNe mV fV fT fP fN lC l) $ \f ->
  classify (simpNonTrivial f) "non-trivial" $
  let fsub = Ex9.simpConj (toExConj f)
      fsol = SEx9.simpConj f
  in length fsub ==? length fsol .&&. (sort $ toSExConj $ fsub) ==? (sort $ fsol)


v1 = SEx9.Pos (SEx9.Var "v1")
nv1 = SEx9.Neg (SEx9.Var "v1")
v2 = SEx9.Pos (SEx9.Var "v2")
v5 = SEx9.Pos (SEx9.Var "v5")

hiddenSimpConj = 
    [
        (toExConj [[SEx9.top, v1],[],[SEx9.top]], [[]]),
        (toExConj [[SEx9.top, v1],[SEx9.bottom]], [[]]),
        (toExConj [[SEx9.bottom, v1],[SEx9.bottom]], [[]]),
        (toExConj [[SEx9.bottom, v1],[SEx9.top]], [[v1]]),
        (toExConj [[SEx9.top, v1],[v2]], [[v2]]),
        (toExConj [[v1, nv1],[v1, nv1]], [[v1, nv1],[v1, nv1]]),
        (toExConj [[SEx9.top, v1],[v5, SEx9.top]], []),
        ([], [])
    ]

prop_simpConjHidden = map (\(f,res) -> ("Testing simpConj with hidden formula", True QC.==> (sort (toSExConj (Ex9.simpConj f))) ==? res)) hiddenSimpConj

prop_conjToFormToConj mV fV fT fP fN lC l = QC.forAll (genConjNe mV fV fT fP fN lC l) $ \f ->
  classify (simpNonTrivial f) "non-trivial" $
  (sort $ SEx9.simpConj $ toSExConj $ Ex9.cnf $ toExForm $ SEx9.conjToForm f) ==? (sort $ SEx9.simpConj f)

-- isInCnf :: SEx9.Form -> Bool
-- isInCnf f = aux f False -- flag indicates if we saw an or already
  -- where
    -- aux (SEx9.L _) _ = True
    -- aux ((SEx9.:&:) f1 f2) False = aux f1 False && aux f2 False
    -- aux ((SEx9.:&:) f1 f2) True = False
    -- aux ((SEx9.:|:) f1 f2) b = aux f1 b && aux f2 b

varsAtom :: SEx9.Atom -> [SEx9.Name]
varsAtom SEx9.T = []
varsAtom (SEx9.Var n) = [n]

varsLiteral :: SEx9.Literal -> [SEx9.Name]
varsLiteral (SEx9.Pos a) = varsAtom a
varsLiteral (SEx9.Neg a) = varsAtom a

varsClause :: SEx9.Clause -> [SEx9.Name]
varsClause = foldl (\acc a -> varsLiteral a ++ acc) []

varsConj :: SEx9.ConjForm -> [SEx9.Name]
varsConj = foldl (\acc a -> varsClause a ++ acc) []

satNonTrivial :: SEx9.ConjForm -> Bool
satNonTrivial f = let v = varsConj f in
  simpNonTrivial f && 
  (SEx9.substConj (zip v $ repeat True) f) /= [] &&
  (SEx9.substConj (zip v $ repeat False) f) /= [] 

prop_cnfEquiSat mV fV fT fP fN n = QC.forAll (genForm mV fV fT fP fN n) $ \f ->
  classify (satNonTrivial $ SEx9.cnf f) "non-trivial" $
  (isJust $ SEx9.satConj $ toSExConj $ Ex9.cnf $ toExForm f) ==? (isJust $ SEx9.sat f)

isInAtom :: SEx9.Name -> SEx9.Atom -> Bool
isInAtom _ SEx9.T = False
isInAtom n (SEx9.Var m) = n == m

isInLiteral :: SEx9.Name -> SEx9.Literal -> Bool
isInLiteral n (SEx9.Pos a) = isInAtom n a
isInLiteral n (SEx9.Neg a) = isInAtom n a

isInClause :: SEx9.Name -> SEx9.Clause -> Bool
isInClause n = or . map (isInLiteral n)

isInConj :: SEx9.Name -> SEx9.ConjForm -> Bool
isInConj n = or . map (isInClause n)

hasNoVariableAtom :: SEx9.Atom -> Bool
hasNoVariableAtom = (==SEx9.T)

hasNoVariableLiteral :: SEx9.Literal -> Bool
hasNoVariableLiteral (SEx9.Pos a) = hasNoVariableAtom a
hasNoVariableLiteral (SEx9.Neg a) = hasNoVariableAtom a

hasNoVariableClause :: SEx9.Clause -> Bool
hasNoVariableClause = and . map hasNoVariableLiteral

hasNoVariableConj :: SEx9.ConjForm -> Bool
hasNoVariableConj = and . map hasNoVariableClause

prop_selectVInF mV fV fT fP fN lC l = QC.forAll (genConj mV fV fT fP fN lC l) $ \f ->
  case Ex9.selectV (toExConj f) of
    Nothing -> hasNoVariableConj f ==? True
    Just (v,_) -> isInConj v f ==? True

prop_satConj mV fV fT fP fN lC l = QC.forAll (genConjNe mV fV fT fP fN lC l) $ \f ->
  classify (satNonTrivial f) "non-trivial" $
  case Ex9.satConj (toExConj f) of
    Nothing -> Nothing ==? SEx9.satConj f
    Just val -> (SEx9.simpConj $ SEx9.substConj val f) ==? []

prop_sat mV fV fT fP fN n = QC.forAll (genForm mV fV fT fP fN n) $ \f ->
  classify (satNonTrivial $ SEx9.cnf f) "non-trivial" $
  case Ex9.sat (toExForm f) of
    Nothing -> Nothing ==? SEx9.sat f
    Just val -> (SEx9.simpConj $ SEx9.substConj val $ SEx9.cnf f) ==? []

tests :: TestTree
tests = testGroup "Tests" [
    localOption (mkTimeout $ 3 * 10^6) $testGroup "Timeout 3 seconds" [
      QC.testProperty "Testing simpConj against model solution: variables and boolean values: Run 1" (prop_simpConj 50 10 1 1 1 30 1000),
      QC.testProperty "Testing simpConj against model solution: variables and boolean values: Run 2" (prop_simpConjNe 30 10 1 1 1 20 2000),
      QC.testProperty "Testing simpConj against model solution: only variables" (prop_simpConjNe 100 1 0 1 1 20 200),
      QC.testProperties "Testing against hidden formulas" prop_simpConjHidden,
      localOption (QC.QuickCheckTests 10) $ QC.testProperty "Testing simpConj against model solution: large inputs" (prop_simpConj 5000 7 1 1 1 250 40000),
      QC.testProperty "Testing if 'simpConj = simpConj . cnf . conjToForm" (prop_conjToFormToConj 60 100 1 1 1 20 500),
      QC.testProperty "Testing if cnf returns an equisatisfiable formula" (prop_cnfEquiSat 50 50 1 1 1 4),
      localOption (QC.QuickCheckTests 10) $ QC.testProperty "Testing if cnf returns an equisatisfiable formula: large inputs" (prop_cnfEquiSat 60 50 1 1 1 5)
    ],
    localOption (mkTimeout $ 2 * 10^6) $testGroup "Timeout 2 seconds" [
      QC.testProperty "Testing if selectV selects a valid variable" (prop_selectVInF 70 1 1 1 1 7 1000),
      localOption (QC.QuickCheckTests 10) $ QC.testProperty "Hidden tests selectV" (prop_selectVInF 30 0 1 1 1 7 10), -- tests if Nothing is returned
      QC.testProperty "Testing if satConj returns valid valuation or Nothing" (prop_satConj 15 10 1 1 1 3 30),
      localOption (QC.QuickCheckTests 20) $ QC.testProperty "Testing if satConj returns valid valuation or Nothing: large inputs" (prop_satConj 30 90 1 1 1 13 600),
      QC.testProperty "Testing if sat returns valid valuation or Nothing" (prop_sat 30 20 1 1 1 4),
      localOption (QC.QuickCheckTests 50) $ QC.testProperty "Testing if sat returns valid valuation or Nothing: large inputs" (prop_sat 200 30 1 1 1 5),
      localOption (QC.QuickCheckTests 4) $ QC.testProperty "Stress test: Not needed for homework point" (prop_sat 160 50 1 1 1 6)
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


{-# LANGUAGE CPP #-}
module Test where

import qualified Interface as Sub
import qualified Solution as Sol
import qualified TypesSolution as SolT
import Generators

import Test.Tasty
import Test.Tasty.Runners.AntXML 
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Assertions
import Test.QuickCheck
import Test.Tasty.HUnit

import Control.Arrow (Arrow(first,second))
import System.Environment (setEnv)

-- mappings
toSubPolarity :: SolT.Polarity -> Sub.Polarity
toSubPolarity SolT.Pos = Sub.Pos
toSubPolarity SolT.Neg = Sub.Neg

toSolPolarity :: Sub.Polarity -> SolT.Polarity
toSolPolarity Sub.Pos = SolT.Pos
toSolPolarity Sub.Neg = SolT.Neg

toSubELiteral :: SolT.ELiteral -> Sub.ELiteral
toSubELiteral (SolT.Literal p n) = Sub.Literal (toSubPolarity p) n

toSolELiteral :: Sub.ELiteral -> SolT.ELiteral
toSolELiteral (Sub.Literal p n) = SolT.Literal (toSolPolarity p) n

toSubEClause :: SolT.EClause -> Sub.EClause
toSubEClause = map toSubELiteral

toSolEClause :: Sub.EClause -> SolT.EClause
toSolEClause = map toSolELiteral

toSubEConjForm :: SolT.EConjForm -> Sub.EConjForm
toSubEConjForm = map toSubEClause

toSolEValuation :: Sub.EValuation -> SolT.EValuation
toSolEValuation = id

toSubEValuation :: SolT.EValuation -> Sub.EValuation
toSubEValuation = id

toSolEResolve :: Sub.EResolve -> SolT.EResolve
toSolEResolve (Sub.Resolve n k1 k2) = SolT.Resolve n k1 k2

toSubEResolve :: SolT.EResolve -> Sub.EResolve
toSubEResolve (SolT.Resolve n k1 k2) = Sub.Resolve n k1 k2

toSolEProof :: Sub.EProof -> SolT.EProof
toSolEProof (Sub.Refutation rs) = SolT.Refutation $ map toSolEResolve rs
toSolEProof (Sub.Model vs) = SolT.Model $ toSolEValuation vs

toSubEProof :: SolT.EProof -> Sub.EProof
toSubEProof (SolT.Refutation rs) = Sub.Refutation $ map toSubEResolve rs
toSubEProof (SolT.Model vs) = Sub.Model $ toSubEValuation vs

-- SmallChecks

scProps :: TestTree
scProps = localOption (SC.SmallCheckDepth 3) $ testGroup "Checked by SmallCheck" [
    -- SC.testProperty "Testing mapping in C" $ prop_scMap Sub.selectAndReflectC SolT.selectAndReflectC
  ]

-- QuickChecks

-- prop_extractModel :: Int -> Int -> Blind (QC.Positive Int) -> Blind (QC.Positive Int) -> Blind (QC.Positive Int) -> QC.Property
-- prop_extractModel fP fN (Blind (QC.Positive mV)) (Blind (QC.Positive lC)) (Blind (QC.Positive l)) =
  -- QC.forAll (genEKeyConjFormSaturatedSatisfiable mV fP fN lC l) $ \kcs ->
  -- let cs = map snd kcs
      -- scs = Sub.toConjForm $ toSubEConjForm cs
      -- vs = SolT.toValuation $ toSolEValuation $ Sub.toEValuation $ Sub.extractModel scs in
  -- QC.counterexample ("The returned valuation\n" ++ show vs ++ "\nis not a model for the formula")
    -- $ Sol.eval vs $ SolT.toConjForm cs

prop_proofCheckValid fP fN mV lC l =
  QC.forAll (genEConjForm1 mV fP fN lC l) $ \cs ->
  let (p,_) = first SolT.toEProof $ Sol.resolution cs in
  classify (isRefutation p) "refutations" $ 
  QC.counterexample (show p ++ "\nis a valid proof for the given formula") $ Sub.proofCheck (toSubEConjForm cs) $ Sub.toProof $ toSubEProof p
  where
    isRefutation (SolT.Refutation _) = True
    isRefutation _ = False

prop_proofCheckInValid fP fN mV lC l =
  QC.forAllBlind (genEConjForm1 mV fP fN lC l) $ \cs ->
  let (p,_) = first SolT.toEProof $ Sol.resolution cs in
  classify (isRefutation p) "refutations" $ 
  QC.counterexample (show p ++ "\nis not a valid proof for the formula\n" ++ show (wrongCs cs p)) $ not $ Sub.proofCheck (toSubEConjForm $ wrongCs cs p) $ Sub.toProof $ toSubEProof p
  where
    isRefutation (SolT.Refutation _) = True
    isRefutation _ = False
    wrongCs cs p = if not $ isRefutation p then [SolT.Literal SolT.Pos $ show $ mV+1]:cs
      else map (return . SolT.Literal SolT.Neg . show) [1..length cs]

prop_resolution :: Int -> Int -> Int -> Int -> Int -> QC.Property
prop_resolution fP fN mV lC l =
  QC.forAll (genEConjForm1 mV fP fN lC l) $ \cs ->
  let scs = toSubEConjForm cs
      (p,_) = first Sub.toEProof $ Sub.resolution scs in
  classify (isRefutation p) "refutations" $ 
  QC.counterexample (show p ++ "\nis not a valid proof for the given formula") $ Sol.proofCheck cs $ SolT.toProof $ toSolEProof p
  where
    isRefutation (Sub.Refutation _) = True
    isRefutation _ = False

qcProps :: TestTree
qcProps = testGroup "Checked by QuickCheck" [
    -- QC.testProperty "extractModel: only negative literals" $ prop_extractModel 0 1,
    -- QC.testProperty "extractModel: only positive literals" $ prop_extractModel 1 0,
    QC.testProperty "proofCheck: accepts only valid proofs" $ prop_proofCheckValid 1 1 10 8 100 .&&. prop_proofCheckInValid 1 1 10 8 100,
    localOption (QuickCheckReplay $ Just 13) $ localOption (mkTimeout $ 1 * 10^6) $ testGroup "Timeout 1 second" [
      QC.testProperty "resolution: creates correct proofs (tiny formulas)" $ prop_resolution 1 1 3 3 3,
      QC.testProperty "resolution: creates correct proofs (small formulas)" $ prop_resolution 1 1 8 5 35
    ],
    localOption (QuickCheckReplay $ Just 2) $ localOption (mkTimeout $ 5 * 10^6) $ testGroup "Timeout 5 seconds" [
      QC.testProperty "resolution: creates correct proofs (small medium formulas)" $ prop_resolution 1 1 10 8 100
    ],
    localOption (QuickCheckReplay $ Just 3) $ localOption (mkTimeout $ 5 * 10^6) $ testGroup "Timeout 5 seconds" [
      QC.testProperty "resolution: creates correct proofs (medium formulas)" $ withMaxSuccess 5 $ prop_resolution 1 1 14 10 200,
      QC.testProperty "resolution: creates correct proofs (medium large formulas)" $ withMaxSuccess 5 $ prop_resolution 1 1 14 15 300,
      QC.testProperty "resolution: creates correct proofs (large formulas)" $ withMaxSuccess 3 $ prop_resolution 1 1 15 15 800
    ],
    localOption (QuickCheckReplay $ Just 5) $ localOption (mkTimeout $ 10 * 10^6) $ testGroup "Timeout 10 seconds" [
      QC.testProperty "resolution: creates correct proof for XL formula" $ withMaxSuccess 3 $ prop_resolution 1 1 16 15 3000
    ]
  ]

-- UnitTests

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
    -- testCase "extractModel: hidden test 1" $ assertBool "extractModel returns a wrong model"
     -- $ Sol.eval (SolT.toValuation $ toSolEValuation $ Sub.toEValuation $ Sub.extractModel $ Sub.toConjForm $ toSubEConjForm cs1)
     -- $ SolT.toConjForm cs1,
    -- testCase "extractModel: hidden test 2" $ assertBool "extractModel returns a wrong model"
     -- $ Sol.eval (SolT.toValuation $ toSolEValuation $ Sub.toEValuation $ Sub.extractModel $ Sub.toConjForm $ toSubEConjForm cs2)
     -- $ SolT.toConjForm cs2
  ]
  -- where
    -- cs1 = map (map SolT.toELiteral) [[Sol.lPos $ SolT.toName "2"],[Sol.lPos $ SolT.toName "4"],[Sol.lPos $ SolT.toName "2", Sol.lNeg $ SolT.toName "4"],
           -- [Sol.lPos $ SolT.toName "1"],[Sol.lPos $ SolT.toName "2", Sol.lNeg $ SolT.toName "3"], [Sol.lNeg $ SolT.toName "9"],
           -- [Sol.lPos $ SolT.toName "10"]]
    -- cs2 = map (map SolT.toELiteral) [[Sol.lNeg $ SolT.toName "2", Sol.lPos $ SolT.toName "1"], [Sol.lNeg $ SolT.toName "1", Sol.lPos $ SolT.toName "2"]]

-- Final tests wrap up and main

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

main :: IO ()
main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOption timeoutOption tests
  where
    resultsPath = "test-reports/results.xml"
#ifdef PROD 
    testRunner = defaultMainWithIngredients [antXMLRunner]
#else
    testRunner = defaultMain
#endif    
    -- by default, run for 1 second
    timeoutOption = mkTimeout (1 * 10^6)


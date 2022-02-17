module Generators where

import qualified Exercise09 as Sol
import qualified Types as SolT

import Test.QuickCheck as QC

instance Arbitrary SolT.Polarity where
  arbitrary = oneof $ map return [SolT.Pos, SolT.Neg]

-- mV: maximum variable index
genEName :: Int -> Gen SolT.EName
genEName mV = do
  n <- choose (1, mV)
  return $ show n

instance Arbitrary SolT.ELiteral where
  arbitrary = sized $ \n -> do
    p <- arbitrary
    v <- genEName n
    return $ SolT.Literal p v
  shrink (SolT.Literal p n) = map (SolT.Literal p . show) [1..(read n - 1)]

-- fP: frequency of positive literals
-- fN: frequency of negative literals
genELiteral :: Int -> Int -> Int -> Gen SolT.ELiteral
genELiteral mV fP fN = do
  v <- genEName mV
  frequency [(fP, return $ SolT.Literal SolT.Pos v), (fN, return $ SolT.Literal SolT.Neg v)]

-- lC: maximum size of clauses
genEClause :: Int -> Int -> Int -> Int -> Gen SolT.EClause
genEClause mV fP fN lC = resize lC $ listOf $ genELiteral mV fP fN

-- same as genClause but generates only non-empty clauses
genEClause1 :: Int -> Int -> Int -> Int -> Gen SolT.EClause
genEClause1 mV fP fN lC = resize lC $ listOf1 $ genELiteral mV fP fN

-- lC: maximum number of clauses
genEConjForm :: Int -> Int -> Int -> Int -> Int -> Gen SolT.EConjForm
genEConjForm mV fP fN lC l = resize l $ listOf $ genEClause mV fP fN lC

-- same as genConjForm but generates only non-empty conjunctions
genEConjForm1 :: Int -> Int -> Int -> Int -> Int -> Gen SolT.EConjForm
genEConjForm1 mV fP fN lC l = resize l $ listOf1 $ genEClause1 mV fP fN lC

genEKeyConjFormSaturated :: Int -> Int -> Int -> Int -> Int -> Gen (SolT.EProof, SolT.EKeyConjForm)
genEKeyConjFormSaturated mV fP fN lC l = do
  cs <- genEConjForm1 mV fP fN lC l 
  let (p, kcs) = Sol.resolution $ SolT.toConjForm cs
  return (SolT.toEProof p, SolT.toEKeyConjForm kcs)

genEKeyConjFormSaturatedSatisfiable :: Int -> Int -> Int -> Int -> Int -> Gen SolT.EKeyConjForm
genEKeyConjFormSaturatedSatisfiable mV fP fN lC l = genEKeyConjFormSaturated mV fP fN lC l `suchThatMap` satisfiableKeyConjForm
  where
    satisfiableKeyConjForm (SolT.Model _, kcs) = Just kcs
    satisfiableKeyConjForm _ = Nothing


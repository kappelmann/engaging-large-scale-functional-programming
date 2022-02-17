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

import Data.Bifunctor
import Data.List
import qualified Data.Bits as B
import Data.Typeable (Typeable)
import Text.Printf

-- Decomposition exercise

decompositionVolume :: Integer -> [Integer] -> Integer
decompositionVolume dims = sum . zipWith (\i x -> x * 2 ^ (i * dims)) [0..]

decompose :: [Integer] -> [Integer]
decompose [] = []
decompose ds = let d = minimum ds in go (product ds) (d : delete d ds)
  where n = length ds
        go vol ds
          | head ds == 0 = []
          | otherwise    =
              let ds'    = map (`B.shiftR` 1) ds
                  vol'   = product ds'
                  nCubes = vol - vol' `B.shiftL` n
              in  nCubes : go vol' ds'

propDecomposeAux :: Bool -> [Integer] -> [Integer] -> Either String String
propDecomposeAux simplePerms xs res
  | not (null res) && last res == 0 = Left $ "Result contains trailing zeroes."
  | any (<0) res = Left $ "Result contains negative numbers."
  | vol1 /= vol2 = Left $ printf "Original cuboid has volume %d, but the decomposition has volume %d." vol1 vol2
  | not (null perms) = Left $ printf "Permuted input %s gives different answer:\n  %s" (show (fst (head perms))) (show (snd (head perms)))
  | res /= corr = Left $ "Wrong answer."
  | otherwise = Right ""
  where corr = Sol.decompose xs
        vol1 = product xs
        vol2 = decompositionVolume (genericLength xs) res
        perms = [(xs', res') | xs' <- if simplePerms then filter (/= xs) [sort xs] else tail (permutations xs), let res' = Sub.decompose xs', res /= res']

tag :: String -> String -> String -> Bool -> Either String String
tag input sub sol b = if b then Right "" else Left msg
  where msg = printf "Input:\n  %s\nExpected output:\n  %s\nYour output:\n  %s" input sol sub

propDecomposeCube :: Integer -> Integer -> Either String String
propDecomposeCube dims sz = tag input' (show sub) (show sol) $ sub == sol
  where input :: [Integer]
        input = genericReplicate dims (2 ^ sz)
        input' :: String
        input' = printf "%d-dimensional cube of size 2^%d\n  %s" dims sz (show input)
        sub = Sub.decompose input
        sol = genericReplicate sz 0 ++ [1]
        
propDecomposeCubeSC (SCS.Positive dims) (SCS.NonNegative sz) = propDecomposeCube dims sz

propDecomposeCubeQC dimBounds bound = forAllShrinkBlind gen shr $ \(dims,sz) -> mkQC (propDecomposeCube dims sz)
  where gen =
          do dims <- choose dimBounds
             sz <- choose (0,bound)
             return (dims, sz)
        shr (dims, sz) = filter (\(dims, sz) -> dims >= fst dimBounds && sz >= 0) [(dims - 1, sz), (dims, sz - 1)]

propDecomposeRow :: Integer -> Integer -> Integer -> Integer -> Either String String
propDecomposeRow dims axis a sz = tag input' (show sub) (show sol) $ sub == sol
  where sz' = 2 ^ sz
        input = genericReplicate axis sz' ++ [a * sz'] ++ genericReplicate (dims - axis - 1) sz'
        input' :: String
        input' = printf "%d-dimensional cuboid with width %d*2^%d in dimension 2^%d and width 2^%d in every other dimension\n  %s" dims a sz (axis+1) sz (show input)
        sub = Sub.decompose input
        sol = genericReplicate sz 0 ++ [a]

propDecomposeRowQC dimBounds bound = forAllShrinkBlind gen shr $ \(dims,axis,a,sz) -> mkQC (propDecomposeRow dims axis a sz)
  where gen =
          do dims <- choose dimBounds
             sz <- choose (0,bound)
             axis <- choose (0, dims - 1)
             a <- choose (2, 9)
             return (dims, axis, a, sz)
        shr (dims, axis, a, sz) = filter (\(dims, axis, a, sz) -> dims >= fst dimBounds && axis >= 0 && axis < dims && a >= 2 && sz >= 0)
          [(dims - 1, axis, a, sz), (dims - 1, axis - 1, a, sz), (dims, axis - 1, a, sz), (dims, axis, a - 1, sz), (dims, axis, a, sz - 1)]


propDecompose :: Bool -> [Integer] -> Either String String
propDecompose simplePerms xs = bimap (\x -> msg ++ x ++ msg2) id $ propDecomposeAux simplePerms xs res
  where res = Sub.decompose xs
        msg = printf "Input:\n  %s\nYour output:\n  %s\n" (show xs) (show res)
        msg2 = printf "\nCorrect output would have been:\n  %s" (show (decompose xs))

mkQC :: Either String String -> QC.Property
mkQC (Right x) = counterexample x True
mkQC (Left x) = counterexample x False

propDecomposeQC :: Bool -> [Integer] -> QC.Property
propDecomposeQC simplePerms xs =
  case propDecompose simplePerms xs of
    Right x -> counterexample x True
    Left x -> counterexample x False

propDecomposeSC1 (SCS.NonNegative x) = propDecompose False [x]
propDecomposeSC2 (SCS.NonNegative x, SCS.NonNegative y) = propDecompose False [x, y]
propDecomposeSC3 (SCS.NonNegative x, SCS.NonNegative y, SCS.NonNegative z) = propDecompose False [x, y, z]

shrinkCuboid' xs = filter (\ys -> not (null ys) && all (>= 0) ys && ys /= xs) $ mapM shrinkIntegral xs
shrinkCuboid (l, u) = filter (\ys -> not (null ys) && all (>= 0) ys && let len = length ys in len >= l && len <= u) . shrinkList shrinkIntegral

propDecomposeFixedDimQC simplePerms bound dim = forAllShrinkBlind (vectorOf dim (choose (0, bound))) shrinkCuboid' (propDecomposeQC simplePerms)
propDecomposeAllDimsQC simplePerms bound dimBounds = forAllShrinkBlind gen (shrinkCuboid dimBounds) (propDecomposeQC simplePerms)
  where gen = do dim <- choose dimBounds
                 vectorOf dim (choose (0, bound))


decomposeSCTests = localOption (SC.SmallCheckDepth 10) $ testGroup "Checked by SmallCheck" [
    localOption (SC.SmallCheckDepth 200) $ SC.testProperty "Testing 1-dimensional cuboids" $ propDecomposeSC1,
    SC.testProperty "Testing 2-dimensional cuboids" $ propDecomposeSC2,
    SC.testProperty "Testing 3-dimensional cuboids" $ propDecomposeSC3,
    SC.testProperty "Testing cubes of size 2^n SC" $ propDecomposeCubeSC
  ]

decomposeQCTests = testGroup "Checked by QuickCheck" [
    QC.testProperty "Testing cubes of size 2^n QC" $ propDecomposeCubeQC (1,3) 5,
    QC.testProperty "Testing \"almost cubic\" cuboids (a row of cubes of size 2^n)" $ propDecomposeRowQC (2,3) 5,
    QC.testProperty "Testing 1-dimensional cuboids (small)" $ propDecomposeFixedDimQC False 50 1,
    QC.testProperty "Testing 2-dimensional cuboids (small)" $ propDecomposeFixedDimQC False 50 2,
    QC.testProperty "Testing 3-dimensional cuboids (small)" $ propDecomposeFixedDimQC False 50 3,
    QC.testProperty "Testing 1-dimensional cuboids (big)" $ propDecomposeFixedDimQC False 1000 1,
    QC.testProperty "Testing 2-dimensional cuboids (big)" $ propDecomposeFixedDimQC False 1000 2,
    QC.testProperty "Testing 3-dimensional cuboids (big)" $ propDecomposeFixedDimQC False 1000 3,
    QC.testProperty "Testing higher-dimensional cuboids (small)" $ propDecomposeAllDimsQC True 50 (4,10),
    QC.testProperty "Testing higher-dimensional cuboids (big)" $ propDecomposeAllDimsQC True 1000 (4,10)
  ]
  
decomposeTests = localOption (mkTimeout (5 * 10 ^ 6)) $ localOption (QC.QuickCheckTests 250) $ testGroup "HA 5.2" [decomposeSCTests, decomposeQCTests]


tests :: TestTree
tests = testGroup "Tests" [decomposeTests]

main :: IO ()
main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOption timeoutOption tests
  where
    resultsPath = "test-reports/results.xml"
    -- by default, run for 1 second
    timeoutOption = mkTimeout (1 * 10^6)
#ifdef LOCAL
    testRunner = defaultMain
#else
    testRunner = defaultMainWithIngredients [antXMLRunner]
#endif

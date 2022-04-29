{-# LANGUAGE FlexibleInstances #-}
module Test where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Copy
import Copy (MonadFileSystem)

import Test.Tasty
import Test.Tasty.QuickCheck

data MockFileSystem = MockFileSystem (Map FilePath String)

instance MonadFileSystem (State MockFileSystem) where
  readFile source = do
    MockFileSystem fs <- get
    return $ fs Map.! source
  writeFile target content = do
    MockFileSystem fs <- get
    put $ MockFileSystem $ Map.insert target content fs

prop_copy = fs Map.! "b.txt" == "hello"
  where initFs = MockFileSystem $ Map.singleton "a.txt" "hello"
        MockFileSystem fs = execState (Copy.copyFile "a.txt" "b.txt") initFs

tests :: TestTree
tests = testGroup "Tests" [testProperty "Testing main" prop_copy]

main = defaultMain tests 

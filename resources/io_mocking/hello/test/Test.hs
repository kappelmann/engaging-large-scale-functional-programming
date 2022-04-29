module Test where

import qualified Hello as Sub

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Mock.System.IO.Internal as Mock

import Control.Monad (when)

user :: String -> Mock.IO ()
user s = do
  Mock.hPutStrLn Mock.stdin s
  output <- Mock.hGetLine Mock.stdout
  when (output /= ("Hello " ++ s))
    (fail $ "\nExpected:\n" ++ "Hello " ++ s ++ "\nActual:\n" ++ output ++ "\n")

prop_hello = forAll (elements ["Karl", "Friedrich", "Rosa"]) $ \s ->
  Mock.evalIO (Mock.setUser (user s) >> Sub.main >> Mock.runUser) Mock.emptyWorld

tests :: TestTree
tests = testGroup "Tests" [testProperty "Testing main" prop_hello]

main = defaultMain tests 

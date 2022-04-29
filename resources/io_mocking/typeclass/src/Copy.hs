{-# LANGUAGE FlexibleInstances #-}
module Copy where

import qualified Prelude
import Prelude hiding (readFile, writeFile)

class Monad m => MonadFileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

copyFile :: MonadFileSystem m => FilePath -> FilePath -> m ()
copyFile source target = do
  content <- readFile source
  writeFile target content

instance MonadFileSystem IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

main :: IO ()
main = do
  copyFile "a.txt" "b.txt"

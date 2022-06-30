{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Competition.Config
import Competition.Types (Submission(..))
import Control.Exception
import Data.Bool
import Data.Maybe
import GHC.IO.Exception
import Prelude hiding (writeFile, readFile, putStrLn)
import System.Directory
import System.FilePath
import System.IO.Error
import System.IO.UTF8
import System.Process

main :: IO ()
main = updateSubmissions

createInterface :: Submission -> String
createInterface Submission {user = user, name = commit, submissionId = submissionId, path = path, batch = batch} =
  "module Interface (user, name, path, batch, submissionId, strategy) where\n" ++
  "import qualified " ++ uploadExerciseName ++ " as E\n" ++
  "user = " ++ show user ++ "\n" ++
  "name = " ++ show commit ++ "\n" ++
  "path = " ++ show path ++ "\n" ++
  "batch = " ++ show batch ++ " :: Int\n" ++
  "submissionId = " ++ show submissionId ++ " :: Int\n" ++
  "type StatefulStrategyFunc a = a -> [Double] -> Int -> [[Int]] -> ((Int, Int), a)\n" ++
  "type StatefulStrategy a = (a, StatefulStrategyFunc a)\n" ++
  "forceStrategy :: StatefulStrategy a -> StatefulStrategy a\n" ++
  "forceStrategy = id\n" ++
  "strategy = forceStrategy E.strategyState\n"

writeInterfaceFile :: Submission -> IO ()
writeInterfaceFile sub@Submission {name = commit} =
  writeFile (submissionCodePath commit </> "Interface.hs") $ createInterface sub

patchSubmissionCabalFile :: String -> IO ()
patchSubmissionCabalFile commit = do
  content <- readFile (submissionPath commit </> "exercise.cabal")
  writeFile (submissionPath commit </> submissionModule commit ++ ".cabal") . unlines . patchSubmissionCabalActual commit . lines $ content
  removeFile (submissionPath commit </> "exercise.cabal")

patchSubmissionCabalActual :: String -> [String] -> [String]
patchSubmissionCabalActual commit (l:r)
  | "name: " == l = ("name: " ++ submissionModule commit) : r
  | otherwise     = l : patchSubmissionCabalActual commit r

saveRead :: FilePath -> String -> IO String
saveRead path fallback = catch (readFile path) (\(_ :: IOException) -> return fallback)

importSubmission :: Int -> String -> IO (Maybe String)
importSubmission batchId userName = do
  -- path equals user
  commit <- readFile (uploadBase </> userName </> "commit")
  exists <- doesDirectoryExist (submissionPath commit)
  if exists then return $ Just commit
  else cleanupOnFailure commit do
    putStrLn $ "Importing submission for " ++ userName
    copyDir templatePath (submissionPath commit)
    createDirectoryIfMissing True $ submissionCodePath commit
    copyDir (uploadBase </> userName </> uploadSourcePath) (submissionCodePath commit)
    submissionId <- incrementAndPersistCounter ".submissionId"
    user <- saveRead (uploadBase </> userName </> "name") userName
    writeInterfaceFile Submission {
        path         = 'S' : commit,
        submissionId = submissionId,
        name         = commit,
        user         = user,
        batch        = batchId,
        strategy     = undefined
      }
    patchSubmissionCabalFile commit
    -- build the submission with our environment,
    -- if it fails this throws and we remove the
    -- submission directory to ensure the competition can build
    callProc $ (proc "stack" ["build"]) { cwd = Just $ submissionPath commit }
    return $ Just commit

callProc :: CreateProcess -> IO ()
callProc proc = do
    exit_code <- withCreateProcess proc \_ _ _ p -> waitForProcess p
    case exit_code of
      ExitSuccess   -> return ()
      ExitFailure r -> ioError (mkIOError OtherError ("callProcess: " ++ show (cmdspec proc) ++ " (exit " ++ show r ++ ")") Nothing Nothing)

cleanupOnFailure :: String -> IO (Maybe String) -> IO (Maybe String)
cleanupOnFailure commit = handle \e -> do
  putStrLn $ "Failed to import Submission " ++ commit ++ ": " ++ show (e :: SomeException)
  _ :: Either IOException () <- try $ removeDirectoryRecursive (submissionPath commit)
  return Nothing

writeLists :: [String] -> IO ()
writeLists commits = do
  let genListImportFilePath = foldr (</>) "" ["src", "Competition", "SubmissionList.Imports.generated"]
  writeFile genListImportFilePath $ unlines $ map ((\n -> "import qualified \"" ++ n ++ "\" Interface as " ++ n ) . submissionModule) commits

  let genListFilePath = foldr (</>) "" ["src", "Competition", "SubmissionList.generated"]
  writeFile genListFilePath $ unlines $ map (submissionListEntry . submissionModule) commits

  patchCompetitionCabalFile commits
  patchStackYaml commits

submissionListEntry :: String -> String
submissionListEntry n =
  " ,Submission {" ++
  " submissionId = " ++ n ++ ".submissionId," ++
  " user = " ++ n ++ ".user," ++
  " name = " ++ n ++ ".name," ++
  " path = " ++ n ++ ".path," ++
  " batch = " ++ n ++ ".batch," ++
  " strategy = wrapStrategyEvaluation " ++ n ++ ".strategy " ++
  "}"

patchCompetitionCabalFile :: [String] -> IO ()
patchCompetitionCabalFile commits = do
  content <- readFile "competition-runner.cabal"
  writeFile "competition-runner-new.cabal" . unlines . patchCompetitionCabalActual commits . lines $ content
  renameFile "competition-runner-new.cabal" "competition-runner.cabal"

patchCompetitionCabalActual :: [String] -> [String] -> [String]
patchCompetitionCabalActual commits (l:r)
  | "-- begin generated code" == l = l : map ((++ ",") . ("    " ++) . submissionModule) commits ++ dropWhile (/= "-- end generated code") r
  | otherwise                      = l : patchCompetitionCabalActual commits r

patchStackYaml :: [String] -> IO ()
patchStackYaml commits = do
  content <- readFile "stack.yaml"
  writeFile "stack_new.yaml" . unlines . patchStackYamlActual commits . lines $ content
  renameFile "stack_new.yaml" "stack.yaml"

patchStackYamlActual :: [String] -> [String] -> [String]
patchStackYamlActual commits (l:r)
  | "packages:" == l = l : "  - ." : map (("  - ." </>) . submissionPath) commits
  | otherwise        = l : patchStackYamlActual commits r

incrementAndPersistCounter :: String -> IO Int
incrementAndPersistCounter fileName = do
  let filePath = submissionBase </> fileName
  exists <- doesFileExist filePath
  n <- if exists
    then (1 +) . read <$> readFile filePath
    else return 1
  createDirectoryIfMissing True submissionBase
  -- seq required to finish the read before the write starts
  n `seq` writeFile filePath $ show n
  return n

updateSubmissions :: IO ()
updateSubmissions = do
  batchId <- incrementAndPersistCounter ".batch"
  putStrLn $ "Importing Submissions; batch " ++ show batchId
  createDirectoryIfMissing True uploadBase
  getDirectoryContents uploadBase >>= mapM (importSubmission batchId) . filterDot >>= writeLists . catMaybes

filterDot :: [String] -> [String]
filterDot = filter ((/=) '.' . head)

-- originally from https://stackoverflow.com/q/6807025
{-# ANN copyDir "HLint: Warning: Redundant <$>" #-} --" #-} -- some crazy stuff to make hlint, ghc and the highlighter happy
copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing True dst
  filterDot <$> getDirectoryContents src >>= mapM_ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    doesDirectoryExist srcPath >>= bool (copyFile srcPath dstPath) (copyDir srcPath dstPath)

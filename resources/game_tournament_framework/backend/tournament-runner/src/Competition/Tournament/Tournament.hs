{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Competition.Tournament.Tournament (runTournament) where

import Competition.ComputeParallel
import Competition.Config
import Competition.MetadataWriter
import Competition.Stats (computeTournamentStats)
import Competition.SubmissionList (submissions)
import Competition.Tournament.Logger
import Competition.Types (Encounter (..), Game (..), Submission (..), Tournament (..), TournamentStats (..), User (..))
import Competition.Util (chunks, shuffle)
import Control.Applicative (liftA2)
import Control.Exception (SomeException, IOException, try, catch)
import Control.Monad (join)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.Either (partitionEithers, rights)
import Data.Functor ((<&>))
import qualified Data.HashSet as HS
import Data.List (intercalate)
import qualified Data.Map.Strict as MS
import Data.Time (UTCTime, getCurrentTime)
import Data.Traversable (for)
import Prelude hiding (readFile, writeFile)
import System.Directory (doesFileExist, createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.IO.UTF8
import System.Process (readProcessWithExitCode)
import Text.Read (readEither)
import System.FilePath ((</>))

type GameResult = Either String Game

runTournament :: IO ()
runTournament = do
  date <- getCurrentTime
  let encounters = matchSubmissions submissions
  cachedResults <- rights <$> mapM readCachedResult encounters
  let handleResults = handleTournamentResults date ((length submissions - 1) * 2 * repeatGames) . (++ map Right cachedResults)
  let ids = HS.fromList $ map (gameId :: Game -> String) cachedResults
  openEncounters <- shuffle $ filter (not . flip HS.member ids . uid) encounters 
  logTournamentStart openEncounters cachedResults
  handleResults =<< runInParallel (60, 0) handleResults parallelGames runGameInDedicatedProcess openEncounters

--- Create matches, each strategy currently also plays against itself.
matchSubmissions :: [Submission] -> [Encounter]
matchSubmissions = zipWith (\l e -> e { num = l }) [1 ..] . concatMap toEncounter . match
  where
    match submissions = [[x, y] | x <- submissions, y <- submissions, user x /= user y]
    toEncounter submissions = [1 .. repeatGames] <&> \n -> Encounter { num = 0, submissionList = submissions, uid = gameId n submissions }
    gameId n = (++ '_' : show n) . intercalate "_" . map (show . submissionId)

readCachedResult :: Encounter -> IO GameResult
readCachedResult encounter@Encounter { submissionList = submissions, uid = uid } = do
  createDirectoryIfMissing True "game-cache"
  let cacheFile = "game-cache" </> uid
  join . readEither <$> safeRead cacheFile ""

safeRead :: FilePath -> String -> IO String
safeRead path fallback = doesFileExist path >>= bool (return fallback) (catch (readFile path) (\(_ :: IOException) -> return fallback))

runGameInDedicatedProcess :: Encounter -> IO GameResult
runGameInDedicatedProcess encounter@Encounter { submissionList = submissions, uid = uid } = do
  logGameStart encounter
  bin <- getExecutablePath
  let commits = map (name :: Submission -> String) submissions
  processResult <- readProcessWithExitCode bin commits ""
  let gameResult = handleGameProcessResult processResult
  logGameEnd encounter gameResult
  let r' = second (\g -> g { gameId = uid } :: Game) gameResult
  createDirectoryIfMissing True "game-cache"
  let cacheFile = "game-cache" </> uid
  writeFile cacheFile (show (r' :: GameResult))
  return r'

handleGameProcessResult :: (ExitCode, String, String) -> GameResult
handleGameProcessResult (ExitSuccess, out, _) = parseGameOutput out
handleGameProcessResult (ExitFailure _, _, err) = Left err

-- 'first' (from Either's Bifunctor instance) is used to map left value to
-- constant error message, overriding the default read error message.
parseGameOutput :: String -> GameResult
parseGameOutput = first (const "Read error") . readEither

handleTournamentResults :: UTCTime -> Int -> [GameResult] -> IO ()
handleTournamentResults start totalGamesPlayer gameEithers = do
  let (errors, games) = partitionEithers gameEithers
  logTournamentResults errors games
  createDirectoryIfMissing True (wwwroot ++ "/data/games")
  mapM_ writeGame games
  _ <- try $ removeDirectoryRecursive (wwwroot ++ "/data/games-pages") :: IO (Either SomeException ())
  let pages = chunks 1000 $ map makePage games
  createDirectoryIfMissing True (wwwroot ++ "/data/games-pages")
  mapM_ writePage $ zip pages [1 ..]
  -- create user pages
  createDirectoryIfMissing True (wwwroot ++ "/data/users")
  let userData = map (makeUser . collectUserGames games) submissions
  mapM_ writeUser userData
  -- create animations directory
  createDirectoryIfMissing True (wwwroot ++ "/data/animations")
  -- write latest submission lookup
  writeLatestUsers $ MS.fromList $ map (\(User {name = n}, id) -> (n, id)) userData
  -- create index page
  writeTournament $ Tournament {date = start, ranking = map (makeRank totalGamesPlayer) userData, stats = computeTournamentStats userData games, pages = length pages}

module Competition.Tournament.Logger
  ( logTournamentStart,
    logTournamentResults,
    logGameStart,
    logGameEnd,
  )
where

import Competition.Logger (logStderr, logStdout)
import Competition.Types (Encounter(..), Game (..), Submission (..))
import Data.List (intercalate)

logTournamentStart :: [Encounter] -> [Game] -> IO ()
logTournamentStart encounters done = do
  logStdout "Tournament started!"
  logStdout $ show (length done) ++ " cached results reused; " ++ show (length encounters) ++ " games to go..."

logTournamentResults :: [String] -> [Game] -> IO ()
logTournamentResults [] games = logStdout $ "Tournament completed successfully after " ++ show (length games) ++ " game(s)."
logTournamentResults errors _ = logStderr $ "Tournament completed, " ++ show (length errors) ++ " error(s)."

logGameStart :: Encounter -> IO ()
logGameStart enc@Encounter { submissionList = submissions } =
  logStdout $
    encounterIdLog enc
      ++ " Game: "
      ++ intercalate " vs. " (map user submissions)
      ++ " ("
      ++ intercalate ", " (map name submissions)
      ++ ")..."

logGameEnd :: Encounter -> Either String Game -> IO ()
logGameEnd enc (Left err) = logStderr $ encounterIdLog enc ++ " errored: " ++ err ++ "."
logGameEnd enc (Right Game {winner = w, reason = r}) =
  logStdout $
    encounterIdLog enc ++ " finished, winner: " ++ show w ++ ", reason: " ++ r ++ "."

encounterIdLog :: Encounter -> String
encounterIdLog Encounter { num = id } = "(" ++ show id ++ ")"

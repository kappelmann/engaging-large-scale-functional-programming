module Competition.Config where

import Competition.Types
import System.FilePath

wwwroot :: FilePath
wwwroot = ".." </> "www"

submissionBase :: FilePath
submissionBase = "submissions"

uploadBase :: String
uploadBase = "uploads"

uploadExerciseName :: String
uploadExerciseName = "Exercise08"

uploadSourcePath :: String
uploadSourcePath = "."

templatePath :: String
templatePath = "template"

submissionPath :: String -> FilePath
submissionPath commit = submissionBase </> 'S' : commit

submissionCodePath :: String -> FilePath
submissionCodePath = submissionPath

submissionModule :: String -> String
submissionModule = (:) 'S'

boardSize :: Size
boardSize = (9, 6)

-- Move timeout in milliseconds
moveTimeoutMillis :: Int
moveTimeoutMillis = 10^3

-- Number of games to be run in parallel
parallelGames :: Int
parallelGames = 2

-- Number of times each pair of submissions of simulated to reduce influence of random strategies
repeatGames :: Int
repeatGames = 1

-- Number of top ranking results to show for tournament stats
tournamentStatTopResults :: Int
tournamentStatTopResults = 10

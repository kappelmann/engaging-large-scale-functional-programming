{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Competition.Types where

import Data.Aeson (ToJSON)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

{- TYPES FROM EXERCISE -}

-- Player is either 1 or -1
type Player = Int

-- absolute value is the number of pieces on the field
-- field belongs to to player with the same sign
type Field = Int
type Row = [Field]
type Column = [Field]

-- boards are rectangles represented as a list of rows
type Board = [Row]

-- a position on the board represented as (row, column)
-- (0,0) is the top left corner, coordinate values increase towards the bottom right
type Pos = (Int, Int)

-- a size represented as (height, width)
type Size = (Int, Int)

-- a strategy that determines the next actions for player at a given board state
-- it is also provided an infinite list of random doubles to perform stochastic actions
type StatefulStrategyFunc a = a -> [Double] -> Player -> Board -> (Pos, a)
-- first value is the state object to pass to the first invocation of each game
type StatefulStrategy a = (a, StatefulStrategyFunc a)

{- ADDITIONAL TYPES - NOT FOR STUDENTS -}

type MoveError = String
type WinReason = String

-- move duration in ms
type MoveDuration = Int
type MoveResult = Either MoveError (MoveDuration, Pos)

data MoveEvaluation = NormalMove Board GameStats | WinningMove Player WinReason GameStats
  deriving (Eq, Generic, Show)

type IoStrategy = IO (Player -> Board -> IO MoveResult)

data Submission = Submission
  { submissionId :: Int,
    user :: String,
    name :: String,
    path :: String,
    batch :: Int,
    strategy :: IoStrategy
  }

data Encounter = Encounter { num :: Int, submissionList :: [Submission], uid :: String }

data GameEvaluation = GameEvaluation
  { winner :: Player,
    reason :: String,
    moves :: [Pos],
    stats :: GameStats
  }

data GamePlayer = GamePlayer {name :: String, userId :: Int} deriving (Eq, Generic, Show, Read, ToJSON)

data Game = Game
  { gameId :: String,
    players :: [GamePlayer],
    winner :: Player,
    reason :: String,
    moves :: [Pos],
    stats :: GameStats
  }
  deriving (Eq, Generic, Show, Read, ToJSON)

data PageEntry = PageEntry {players :: [String], winner :: Player, gameId :: String} deriving (Eq, Generic, Show, ToJSON)
data UserGame = UserGame {gameId :: String, enemy :: String, result :: Int} deriving (Eq, Generic, Show, ToJSON)
data User = User {name :: String, userId :: Int, commit :: String, games :: [[UserGame]], stats :: UserStats} deriving (Eq, Generic, Show, ToJSON)
data Rank = Rank {userId :: Int, name :: String, score :: Float} deriving (Eq, Generic, Show, ToJSON)
data Tournament = Tournament {date :: UTCTime, ranking :: [Rank], stats :: TournamentStats, pages :: Int} deriving (Eq, Generic, Show, ToJSON)
data UpdateResult = NormalResult Board GameStats | WinResult Player String GameStats
  deriving (Show)

data NumericStat = NumericStat {statSum :: Float, statMin :: Float, statMax :: Float, statAvg :: Float, statVariance :: Float} deriving (Eq, Generic, Show, Read, ToJSON)

data GameStats = GameStats
  { gameFrames :: Int,
    playerStats :: [PlayerStats]
  }
  deriving (Eq, Generic, Show, Read, ToJSON)

-- derived statistics are indicated by "drv" prefix
data PlayerStats = PlayerStats
  { movesMade :: Int,
    orbsCaptured :: Int,
    orbsLost :: Int,
    explosionsTriggered :: Int,
    chainReactionsStarted :: Int,
    targetCellFillings :: [Int],
    numOwnCellsOnBoard :: [Int],
    moveDurations :: [MoveDuration],
    drvKillDeathRatio :: Float,
    drvAvgCapturesPerMove :: Float,
    drvAvgExplosions :: Float,
    drvTargetCellFilling :: NumericStat,
    drvBoardControl :: NumericStat,
    drvCloseCalls :: Int,
    drvMoveDurations :: NumericStat
  }
  deriving (Eq, Generic, Show, Read, ToJSON) 

data UserStats = UserStats
  { gamesWon :: Int,
    gamesLost :: Int,
    movesMade :: NumericStat,
    gameFrames :: NumericStat,
    orbsCaptured :: NumericStat,
    orbsLost :: NumericStat,
    boardControl :: NumericStat,
    killDeathRatio :: NumericStat,
    targetCellFilling :: NumericStat,
    moveDurations :: NumericStat,
    efficiency :: Float
  }
  deriving (Eq, Generic, Show, ToJSON)

data TournamentUserStat = TournamentUserStat
  { userSubmissionId :: Int,
    userId :: Int,
    userCommit :: String,
    userName :: String,
    statValue :: Float,
    rank :: Int
  }
  deriving (Eq, Generic, Show, ToJSON)

data TournamentStats = TournamentStats
  { numGames :: Int,
    numUsers :: Int,
    longestGames :: [Game],
    highestKillDeathRatio :: [TournamentUserStat],
    highestBoardControl :: [TournamentUserStat],
    mostCloseCalls :: [TournamentUserStat],
    mostCloseCallsInWinningGames :: [TournamentUserStat],
    mostEfficient :: [TournamentUserStat]
  }
  deriving (Eq, Generic, Show, ToJSON)

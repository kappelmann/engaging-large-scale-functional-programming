{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Competition.MetadataWriter where

import Competition.Config
import Competition.Stats (computeUserStats)
import Competition.Types
import Competition.Util
import Data.Aeson (ToJSON, encodeFile)
import qualified Data.ByteString.Builder as Bb
import qualified Data.ByteString.Builder.Prim as Bbp
import Data.List
import qualified Data.Map.Strict as MS
import Foreign.Marshal.Utils (fromBool)
import GHC.Generics (Generic)
import System.IO (IOMode (WriteMode), withFile)

-- Reduced game record (without list of moves)
data GameJson = GameJson
  { gameId :: String,
    players :: [GamePlayer],
    winner :: Player,
    reason :: String,
    stats :: GameStats
  }
  deriving (Eq, Generic, ToJSON)

writeGame :: Game -> IO ()
writeGame game@Game {gameId = id, moves = ms} = do
  let basePath = wwwroot ++ "/data/games/" ++ id
  encodeFile (basePath ++ ".json") $ gameToGameJson game
  withFile (basePath ++ ".bin") WriteMode (\file -> Bb.hPutBuilder file $ encodeMoves boardSize ms)
  return ()

gameToGameJson :: Game -> GameJson
gameToGameJson Game {gameId = id, players = ps, winner = w, reason = r, stats = s} = GameJson {gameId = id, players = ps, winner = w, reason = r, stats = s}

makeGamePlayerData :: Submission -> GamePlayer
makeGamePlayerData Submission {user = n, submissionId = i} = GamePlayer {name = n, userId = i}

makePage :: Game -> PageEntry
makePage Game {gameId = id, players = ps, winner = w} = PageEntry {gameId = id, players = map (name :: GamePlayer -> String) ps, winner = w}

writePage :: ([PageEntry], Int) -> IO ()
writePage (p, n) = encodeFile (wwwroot ++ "/data/games-pages/" ++ show n ++ ".json") p

writeUser :: (User, Int) -> IO ()
writeUser (u, n) = encodeFile (wwwroot ++ "/data/users/" ++ show n ++ ".json") u

collectUserGames :: [Game] -> Submission -> (Submission, [Game], [Game], UserStats)
collectUserGames gs submission@Submission {submissionId = si} = (submission, gamesPlayedAs 0 gs, gamesPlayedAs 1 gs, computeUserStats si $ gamesPlayed gs)
  where
    gamesPlayedAs i = filter (gamePlayedAs i)
    gamePlayedAs i Game {players = ps} = (userId :: GamePlayer -> Int) (ps !! i) == si
    gamesPlayed = filter gamePlayed
    gamePlayed Game {players = ps} = elem si $ map (userId :: GamePlayer -> Int) ps

makeUser :: (Submission, [Game], [Game], UserStats) -> (User, Int)
makeUser (Submission {user = n, name = commit, submissionId = i}, first, last, stats) = (User {name = n, commit = commit, userId = i, games = [map (userGame 1) first, map (userGame (-1)) last], stats = stats}, i)

userGame :: Player -> Game -> UserGame
userGame p Game {gameId = id, players = ps, winner = w} = UserGame {gameId = id, result = fromBool (p == w), enemy = (name :: GamePlayer -> String) (ps !! playerToIndex (- p))}

makeRank :: Int -> (User, Int) -> Rank
makeRank totalGamesPlayer (User {name = n, games = g}, userId) = Rank {userId = userId, name = n, score = rankGames totalGamesPlayer $ concat g}

rankGames :: Int -> [UserGame] -> Float
rankGames totalGamesPlayer games = numWon * numGames / total -- TODO: implement a better scoring function
  where
    numGames = fromIntegral $ length games
    numWon = fromIntegral $ foldl' (+) 0 $ map result games
    total = fromIntegral totalGamesPlayer

writeLatestUsers :: MS.Map String Int -> IO ()
writeLatestUsers = encodeFile (wwwroot ++ "/data/latest.json")

writeTournament :: Tournament -> IO ()
writeTournament = encodeFile (wwwroot ++ "/data/index.json")

encodeMoves :: Size -> [Pos] -> Bb.Builder
encodeMoves (h, w) ps = Bb.word8 1 `mappend` Bb.word8 (fromIntegral h) `mappend` Bb.word8 (fromIntegral w) `mappend` Bbp.primMapListFixed Bbp.word8 (wordMoves ps)
  where
    wordMoves [] = []
    wordMoves ((a, b) : ms) = fromIntegral a : fromIntegral b : wordMoves ms

{-# LANGUAGE DuplicateRecordFields #-}

module Competition.Game.Game (runGame) where

import Competition.Config (boardSize)
import Competition.Stats
import Competition.SubmissionList (submissions)
import Competition.Types
import Competition.Util
import Data.List (find, intercalate)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isNothing)
import System.Exit (die)

runGame :: [String] -> IO ()
runGame commits = do
  let maybeSubmissions = findSubmissions commits
  result <- case maybeSubmissions of
    Nothing -> return (Left "Required submissions not found")
    Just submissions -> Right <$> playGame submissions
  handleGameResult result

findSubmissions :: [String] -> Maybe [Submission]
findSubmissions commits
  | any isNothing submissionMaybes = Nothing
  | otherwise = Just (catMaybes submissionMaybes)
  where
    submissionMaybes = map findSubmission commits

findSubmission :: String -> Maybe Submission
findSubmission commit = find (\sub -> commit == (name :: Submission -> String) sub) submissions

handleGameResult :: Either String Game -> IO ()
handleGameResult (Left error) = die error
handleGameResult (Right game) = print game

-- Play a single game and return its result
playGame :: [Submission] -> IO Game
playGame submissions = do
  let strategies = map strategy submissions
  strategies' <- sequence strategies
  let board = makeBoard boardSize
  let players = [1, -1]
  let infPlayers = cycle players
  let infStrategies = cycle strategies'
  let stats = initGameStats board (length players)
  gameEvaluation <- evaluateGame [] board infPlayers infStrategies stats
  return $ generateGame submissions gameEvaluation

type Strategy = Player -> Board -> IO MoveResult

evaluateGame :: [Pos] -> Board -> [Player] -> [Strategy] -> GameStats -> IO GameEvaluation
evaluateGame ms b (p : ps) (s : ss) st = s p b >>= either fromMoveError fromCorrectMove
  where
    fromMoveError :: MoveError -> IO GameEvaluation
    fromMoveError e = return GameEvaluation {winner = - p, reason = e, moves = ms, stats = st}
    fromCorrectMove :: (MoveDuration, Pos) -> IO GameEvaluation
    fromCorrectMove move@(_, pos) = fromApplyResult move $ applyMove p pos b st
      where
        applyMove :: Player -> Pos -> Board -> GameStats -> MoveEvaluation
        applyMove player move board stats =
          let board' = updateField (updateValue (+ 1) player) move board
              stats' = stPlayerMadeMove stats player move board
          in updateBoard player board' stats'
        fromApplyResult :: (MoveDuration, Pos) -> MoveEvaluation -> IO GameEvaluation
        fromApplyResult (d, pos) (NormalMove board' st') = evaluateGame (pos : ms) board' ps ss (addMoveDuration st' d)
        fromApplyResult (d, pos) (WinningMove player' reason st') = return GameEvaluation {winner = player', reason = reason, moves = pos : ms, stats = addMoveDuration st' d}
        addMoveDuration :: GameStats -> MoveDuration -> GameStats
        addMoveDuration gs@GameStats { playerStats = psLst } d = let pi = playerToIndex p in gs { playerStats = zipWith (\ps idx -> if idx == pi then (ps :: PlayerStats) { moveDurations = (moveDurations :: PlayerStats -> [MoveDuration]) ps ++ [d] } else ps) psLst [0..] }

generateGamePlayer :: Submission -> GamePlayer
generateGamePlayer Submission {submissionId = i, user = n} = GamePlayer {userId = i, name = n}

generateGame :: [Submission] -> GameEvaluation -> Game
generateGame submissions GameEvaluation {winner = w, reason = r, moves = ms, stats = st} =
  Game
    { gameId = "",
      players = map generateGamePlayer submissions,
      winner = w,
      reason = r,
      moves = ms,
      stats = computeDerivedGameStats st
    }

-- the game cannot end without a reaction happening - this also saves us from having to ensure
-- that all players have made a move before the end, since the first reaction can only happen
-- after a player put two orbs in a corner
-- NOTE: this simplification does not apply to boards with cells with less than two neighbours
updateBoard :: Player -> Board -> GameStats -> MoveEvaluation
updateBoard p = go 1000 -- limit to at most 1000 reactions to combat infinite updates
  where
    go :: Int -> Board -> GameStats -> MoveEvaluation
    go 0 _ st = WinningMove (- p) "Loop" st
    go n b st
      | not changed = NormalMove b' st' -- if no reaction happened, the board is stable and we are done
      | gameOver b' = WinningMove p "Won" (st' { gameFrames = (gameFrames :: GameStats -> Int) st' + 1 }) -- if won after reaction sequence, include last frame 
      | otherwise = go (n - 1) b' st'
      where
        (b', st', changed) = shallowlyApplyReactions p b st

gameOver :: Board -> Bool
gameOver = allEqual . map signum . filter (0 /=) . concat
  where
    allEqual [] = False -- no strictly pos. or neg. field descriptor numbers (or empty board)
    allEqual (x : xs) = all (== x) xs

-- update a board once by applying reactions
-- returns the new board with the updated stats and a bool indicating whether there has been a reaction
shallowlyApplyReactions :: Player -> Board -> GameStats -> (Board, GameStats, Bool)
shallowlyApplyReactions player board stats
  | null critical = (board, stats', False)
  | otherwise = (reacted, stats', True)
  where
    s :: Size
    s = size board
    critical :: [(Pos, Field)]
    critical = filter (\(p, v) -> abs v >= criticalMass s p) $ fields board
    toUpdate :: M.Map Pos (Field -> Field)
    toUpdate = foldr collectUpdate M.empty critical
    collectUpdate :: (Pos, Field) -> M.Map Pos (Field -> Field) -> M.Map Pos (Field -> Field)
    collectUpdate (p, _) =
      M.insertWith (.) p (flip (-) (criticalMass s p))
        . foldr ((.) . (\n -> M.insertWith (.) n (+ 1))) id (neighboursList s p)
    reacted :: Board
    reacted = mapBoard doUpdateField board
    doUpdateField :: Pos -> Field -> Field
    doUpdateField p v = maybe v (updateValue' player v) $ M.lookup p toUpdate
    stats' :: GameStats
    stats' = stBoardUpdate stats player board (length critical) orbsCaptured
    orbsCaptured :: Int
    orbsCaptured = M.foldrWithKey (\pos _ acc -> let oldVal = getField pos board in acc + (if signum oldVal == (- player) then abs oldVal else 0)) 0 toUpdate

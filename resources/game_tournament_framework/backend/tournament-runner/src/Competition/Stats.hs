{-# LANGUAGE DuplicateRecordFields #-}

module Competition.Stats where

import Competition.Config
import Competition.Types
import Competition.Util

import Data.List (elemIndex, findIndex, sortBy)
import Data.Maybe (fromMaybe)

--------------------------------
--       Aux. functions       --
--------------------------------

isProblematicFloat :: Float -> Bool
isProblematicFloat fl = isNaN fl || isInfinite fl

sanitize :: Float -> Float -- JSON has no "Infinity"
sanitize fl = if isProblematicFloat fl then maxNonInfiniteFloat (0.0 :: Float) else fl

initNumericStat :: NumericStat
initNumericStat =
  NumericStat
  { statSum = nan,
    statMin = nan,
    statMax = nan,
    statAvg = nan,
    statVariance = nan
  }
  where
    nan = 0.0/0.0

numLst :: [a] -> (a -> Float) -> NumericStat
numLst l f =
  NumericStat
  { statSum = sanitize $ sumLst l f,
    statMin = sanitize $ minLst l f,
    statMax = sanitize $ maxLst l f,
    statAvg = sanitize $ avgLst l f,
    statVariance = sanitize $ varLst l f
  }
  where
    lenOrOne = fromIntegral $ max 1 $ length l
    sumLst l f = sum $ map f l
    minLst l f = let mp = map f l in if null mp then 0 else minimum mp
    maxLst l f = let mp = map f l in if null mp then 0 else maximum mp
    avgLst l f = sumLst l f / lenOrOne
    varLst l f = let (avg, recipr) = (avgLst l f, 1.0 / lenOrOne) in sum [recipr * (f st - avg) ** 2.0 | st <- l]

--------------------------------
--        Player stats        --
--------------------------------

initPlayerStats :: PlayerStats
initPlayerStats =
  PlayerStats
  { movesMade = 0,
    orbsCaptured = 0,
    orbsLost = 0,
    explosionsTriggered = 0,
    chainReactionsStarted = 0,
    targetCellFillings = [],
    numOwnCellsOnBoard = [],
    moveDurations = [],
    drvKillDeathRatio = nan,
    drvAvgCapturesPerMove = nan,
    drvAvgExplosions = nan,
    drvTargetCellFilling = initNumericStat,
    drvBoardControl = initNumericStat,
    drvMoveDurations = initNumericStat,
    drvCloseCalls = 0 }
  where
    nan = 0.0/0.0

computeDerivedPlayerStats :: PlayerStats -> PlayerStats 
computeDerivedPlayerStats st =
  st
  { drvKillDeathRatio = fi (orbsCaptured :: PlayerStats -> Int) / fi (orbsLost :: PlayerStats -> Int),
    drvAvgCapturesPerMove = fi (orbsCaptured :: PlayerStats -> Int) / m,
    drvAvgExplosions = fi explosionsTriggered / m,
    drvTargetCellFilling = numLst (targetCellFillings st) fromIntegral,
    drvBoardControl = numLst (numOwnCellsOnBoard st) ((/numBoardCells) . fromIntegral),
    drvCloseCalls = length $ filter (\(curr, next) -> curr > 1 && next == 1) closeCallAdjacentList, 
    drvMoveDurations = numLst ((moveDurations :: PlayerStats -> [MoveDuration]) st) fromIntegral,
    -- clear lists to avoid bloating JSON files
    -- (but keep move durations)
    targetCellFillings = [],
    numOwnCellsOnBoard = []
  }
  where
    fi = \f -> fromIntegral $ f st
    fm dflt = \f -> fromIntegral $ maximum $ dflt : f st
    numBoardCells = fromIntegral $ uncurry (*) boardSize
    numOwnCellsList = numOwnCellsOnBoard st
    closeCallFilteredList = take (length numOwnCellsList - 2) numOwnCellsList -- exclude last 2 updates
    closeCallAdjacentList = zip closeCallFilteredList (drop 1 closeCallFilteredList)
    m = max (fi movesMade) 1.0

stPlayerMadeMove :: GameStats -> Player -> Pos -> Board -> GameStats
stPlayerMadeMove st p pos b =
  st
  { playerStats = take idx ps ++ [updatePlayerStats (playerStats st !! idx)] ++ drop (idx+1) ps
  }
  where
    ps = playerStats st
    idx = playerToIndex p
    oldAbs = abs $ getField pos b
    updatePlayerStats st' =
      st'
      { movesMade = (movesMade :: PlayerStats -> Int) st' + 1,
        targetCellFillings = targetCellFillings st' ++ [oldAbs],
        chainReactionsStarted = chainReactionsStarted st' + (if criticalMass (size b) pos == oldAbs + 1 then 1 else 0)
      }

--------------------------------
--         Game stats         --
--------------------------------

initGameStats :: Board -> Int -> GameStats
initGameStats _ numPlayers =
  GameStats
  { gameFrames = 1,
    playerStats = replicate numPlayers initPlayerStats
  }

computeDerivedGameStats :: GameStats -> GameStats
computeDerivedGameStats st =
  st
  { playerStats = map computeDerivedPlayerStats (playerStats st)
  }

stBoardUpdate :: GameStats -> Player -> Board -> Int -> Int -> GameStats
stBoardUpdate st p b numCritical numOrbsCaptured =
  st
  { gameFrames = (gameFrames :: GameStats -> Int) st + 1,
    playerStats = zipWith (curry updatePlayerStats) [0..length ps - 1] ps
  }
  where
    ps = playerStats st
    idx = playerToIndex p
    updatePlayerStats (idx', st') =
      st'
      { explosionsTriggered = explosionsTriggered st' + (if idx == idx' then numCritical else 0),
        orbsCaptured = (orbsCaptured :: PlayerStats -> Int) st' + (if idx == idx' then numOrbsCaptured else 0),
        orbsLost = (orbsLost :: PlayerStats -> Int) st' + (if idx /= idx' then numOrbsCaptured else 0),
        numOwnCellsOnBoard = numOwnCellsOnBoard st' ++ [foldr (\row acc -> acc + length (filter ((==p') . signum) row)) 0 b]
      }
      where
        p' = [1, -1] !! idx'

--------------------------------
--         User stats         --
--------------------------------

initUserStats :: UserStats
initUserStats =
  UserStats
  { gamesWon = 0,
    gamesLost = 0,
    movesMade = ns,
    gameFrames = ns,
    orbsCaptured = ns,
    orbsLost = ns,
    boardControl = ns,
    killDeathRatio = ns,
    targetCellFilling = ns,
    moveDurations = ns,
    efficiency = nan
  }
  where
    ns = initNumericStat
    nan = 0.0/0.0

computeUserStats :: Int -> [Game] -> UserStats
computeUserStats id gs =
  initUserStats
  { gamesWon = numGamesWon,
    gamesLost = numGames - numGamesWon,
    movesMade = numSt (fi . (movesMade :: PlayerStats -> Int)),
    gameFrames = numGs (fi . (gameFrames :: GameStats -> Int)),
    orbsCaptured = numSt (fi . (orbsCaptured :: PlayerStats -> Int)),
    orbsLost = numSt (fi . (orbsLost :: PlayerStats -> Int)),
    boardControl = numStFilter (not . isProblematicFloat . statAvg . drvBoardControl) (sanitize . statAvg . drvBoardControl),
    killDeathRatio = numStFilter (not . isProblematicFloat . drvKillDeathRatio) (sanitize . drvKillDeathRatio),
    targetCellFilling = numStFilter (not . isProblematicFloat . statAvg . drvTargetCellFilling) (sanitize . statAvg . drvTargetCellFilling),
    moveDurations = moveDurations,
    efficiency =
      let
        avgMoveDuration = statAvg moveDurations
        winParticipationRatio = fromIntegral numGamesWon / fromIntegral numGames
      in
        if any isProblematicFloat [avgMoveDuration, winParticipationRatio] then 0.0 else (1.0 - avgMoveDuration / fromIntegral moveTimeoutMillis) * winParticipationRatio
  }
  where
    fi i = fromIntegral i :: Float
    numGames = length gs
    numGamesWon :: Int
    numGamesWon = length $ filter (\game -> (map (userId :: GamePlayer -> Int) ((players :: Game -> [GamePlayer]) game) !! playerToIndex ((winner :: Game -> Player) game)) == id) gs
    ownPlayerStats :: Game -> PlayerStats
    ownPlayerStats game@Game {stats = st} = playerStats st !! fromMaybe 0 playerIndexInGame
      where
        playerIndexInGame = findIndex ((==id) . (userId :: GamePlayer -> Int)) $ (players :: Game -> [GamePlayer]) game
    ownPlayerStatsList = map ownPlayerStats gs
    gameStatsList = map (stats :: Game -> GameStats) gs
    moveDurations = numStFilter (not . isProblematicFloat . statAvg . drvMoveDurations) (sanitize . statAvg . drvMoveDurations)
    numSt = numLst ownPlayerStatsList
    numStFilter f = numLst $ filter f ownPlayerStatsList
    numGs = numLst gameStatsList

--------------------------------
--      Tournament stats      --
--------------------------------

computeTournamentStats :: [(User, Int)] -> [Game] -> TournamentStats
computeTournamentStats users games =
  TournamentStats
  { numGames = length games,
    numUsers = length users,
    longestGames =
      let stGameFrames = (gameFrames :: GameStats -> Int) . (stats :: Game -> GameStats)
      in map (\g -> (g :: Game) { moves = [], stats = ((stats :: Game -> GameStats) g) { playerStats = [] } }) $ take n (sortBy (\a b -> compare (stGameFrames b) (stGameFrames a)) games),
    highestKillDeathRatio = makeDescTUSList (statAvg . killDeathRatio . (stats :: User -> UserStats) . fst),
    highestBoardControl = makeDescTUSList (statAvg . boardControl . (stats :: User -> UserStats) . fst),
    mostCloseCalls = stCloseCalls filterGamesParticipated,
    mostCloseCallsInWinningGames = stCloseCalls filterGamesWon,
    mostEfficient = makeDescTUSList (efficiency . (stats :: User -> UserStats) . fst)
  }
  where
    n = tournamentStatTopResults
    stCloseCalls gameFilter = makeDescTUSList (\u -> fromIntegral $ sum $ map (\g -> drvCloseCalls (playerStats ((stats :: Game -> GameStats) g) !! playerIndex g u)) (gameFilter u))
    filterGamesParticipated u = filter (\g -> snd u `elem` map (userId :: GamePlayer -> Int) ((players :: Game -> [GamePlayer]) g)) games
    filterGamesWon u = filter ((==snd u) . gameWinner) games
    playerIndex g u = fromMaybe 0 (elemIndex (snd u) (map (userId :: GamePlayer -> Int) ((players :: Game -> [GamePlayer]) g)))
    winnerIndex g = playerToIndex $ (winner :: Game -> Player) g
    gameWinner g = (userId :: GamePlayer -> Int) ((players :: Game -> [GamePlayer]) g !! winnerIndex g)
    descCompFunc valFunc = \a b -> if isProblematicFloat (valFunc b) then LT else compare (valFunc b) (valFunc a)
    makeTUSList valFunc compFunc = zipWith (userToTUS valFunc) [1..] $ take n $ sortBy compFunc users
    makeDescTUSList valFunc = makeTUSList valFunc (descCompFunc valFunc)
    userToTUS valFunc rank user = TournamentUserStat { userSubmissionId = snd user, userId = (userId :: User -> Int) $ fst user, userCommit = commit $ fst user, userName = (name :: User -> String) $ fst user, statValue = valFunc user, rank = rank }
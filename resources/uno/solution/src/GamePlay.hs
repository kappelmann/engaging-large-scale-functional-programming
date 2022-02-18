module GamePlay where

import Exercise
import InternalUtil
import Util
import Data.List
import Types
import System.Random
import Text.Read
import System.IO.Unsafe
import System.Timeout

-- you can use the playAndPrint method to play your strategy against the simpleStrategy
-- or the interactiveStrategy that lets you play against your own strategy
-- e.g. simply call 'playAndPrint *seed* [unoAI, interactiveStrategy]
-- after the game has finished, you get each single move printed with the according game state
playAndPrint :: Seed -> [Strategy] -> IO ()
playAndPrint seed strats = do
  result <- playWithSeed False seed strats
  case result of
    Result (Just player) games reason -> do
      mapM_ printMove games
      putStrLn $ "\nPlayer " ++ show player ++ " wins!"
      putStrLn $ "Reason: " ++ reason
    Result Nothing games reason -> do
      mapM_ printMove games
      putStrLn $ "\nThe game ties after " ++ show (length games) ++ " moves"
      putStrLn $ "Reason: " ++ reason
  where
    printMove (g, eval) = putStrLn $ "----------------------\n" ++ prettyShowGame g ++ "\nPlayer " ++ show (currentPlayer g) ++ " " ++ show eval ++ "\n"

-- Allows you to test multiple strategies against each other
-- second parameter is the number of games that you want to simulate
playStrategies :: [Strategy] -> Int -> IO ()
playStrategies xs = playShowWinner xs (replicate (length xs) 0) 0
  where
    playShowWinner :: [Strategy] -> [Int] -> Seed -> Int -> IO ()
    playShowWinner strats winCount _ 0 = mapM_ (\c -> print ("Player " ++ show (c + 1) ++ " won " ++ show (winCount !! c) ++ "-times")) [0 .. length strats -1]
    playShowWinner strats winCount startSeed count =
      do
        result <- playWithSeed True (startSeed + count) strats
        case result of
          Result (Just player) _ _ -> let (a,p:b) = splitAt player winCount in
            playShowWinner strats (a ++ (p + 1):b) startSeed (count - 1)
          Result Nothing _ _ -> playShowWinner strats winCount startSeed (count - 1)

{---interactive strategy---}
interactiveStrategy :: Strategy
interactiveStrategy others hand last draw2stack _ =
  unsafePerformIO $ do
    putStrLn "------------\nIt is your turn!"
    putStrLn $ "The other players have the following number of cards: " ++ show others
    putStrLn $ "Last played card: " ++ show last
    putStrLn $ "Your hand: " ++ show hand
    newInput

interactiveDrawStrategy :: DrawStrategy
interactiveDrawStrategy others hand last _ =
  unsafePerformIO $ do
    putStrLn "\n -> You drew a card!"
    putStrLn $ "The other players have the following number of cards: " ++ show others
    putStrLn $ "Last played card: " ++ show last
    putStrLn $ "Your drawn card: " ++ show (head hand)
    putStrLn $ "Your other cards: " ++ show (tail hand)
    newDrawInput

newInput :: IO Move
newInput = do
  putStr "Insert your move ('Play [card]' or 'Draw'): "
  input <- getLine
  maybe newInput return (parseInput input)

newDrawInput :: IO DrawMove
newDrawInput = do
  putStr "Do you want to play the drawn card ('Play [card]') or keep it ('Keep')? "
  input <- getLine
  maybe newDrawInput return (parseDrawInput input)

parseInput :: String -> Maybe Move
parseInput s
  | s == "Draw" || s == "D" = return $ Draw interactiveDrawStrategy
  | "Play " `isPrefixOf` s = do
      card <- parseCard (drop 5 s)
      return $ Play card
  | "P " `isPrefixOf` s = do
      card <- parseCard (drop 2 s)
      return $ Play card
  | otherwise = Nothing

parseDrawInput :: String -> Maybe DrawMove
parseDrawInput s
  | s == "Keep" || s == "K" = return None
  | "Play " `isPrefixOf` s = do
      card <- parseCard (drop 5 s)
      return $ Return card
  | "P " `isPrefixOf` s = do
      card <- parseCard (drop 2 s)
      return $ Return card
  | otherwise = Nothing

parseCard :: String -> Maybe Card
parseCard s
  | "Wild" `isPrefixOf` s = do
      col <- readMaybe (drop 5 s)
      return $ WildCard Wild (Just col)
  | "Draw4" `isPrefixOf` s = do
      col <- readMaybe (drop 6 s)
      return $ WildCard Draw4 (Just col)
  | length s > 3 = do
      col <- readMaybe (take 1 s)
      act <- readMaybe (drop 2 s)
      return $ ActCard col act
  | otherwise = do
      col <- readMaybe (take 1 s)
      num <- readMaybe (drop 2 s)
      return $ NumCard col num

{---------------strategy evaluator-----------------}

playWithSeed :: Bool -> Seed -> [Strategy] -> IO Result
playWithSeed b seed playerStrats =
  go seed [] $ startWithSeed seed (length playerStrats)
  where
    go :: Seed -> [Int] -> Game -> IO Result
    go seed disqualified game@Game {hands = hands, currentPlayer = player}
      | length hands - 1 == length disqualified =
        return $ Result (findIndex (`notElem` disqualified) [0..(length hands - 1)]) [] "==> all other players are disqualified"
      | isGameOver game =
        return $ Result (elemIndex [] hands) [] "==> played all the cards in the hand"
      | null (drawPile game) = return $ Result Nothing [] "==> The Draw Pile is empty"
      | player `elem` disqualified = go seed disqualified game {currentPlayer = nextPlayer (head $ discardPile game) (length hands) player (direction game)}
      | otherwise = do
        (newGame, evaluation) <- evaluateStrategy b seed game (playerStrats !! player)
        case evaluation of
          Err _ -> mappend (Result Nothing [(game, evaluation)] ("Player " ++ show player ++ " got disqualified\n"))
            <$> go (seed + 1) (player : disqualified) game
          _ -> mappend (Result Nothing [(game, evaluation)] "") <$> go (seed + 2) disqualified newGame

timeLimit :: Int
timeLimit = 10 ^ 6

evaluateStrategy :: Bool -> Seed -> Game -> Strategy -> IO (Game, Evaluation)
evaluateStrategy False seed game@Game { currentPlayer = player } strat = do
  let players  = deckSizes game
      hand     = hands game !! currentPlayer game
      lastCard = head $ discardPile game
      d2stack  = draw2stack game
      rdmInput = randoms $ mkStdGen seed
  move <- return $! strat players hand lastCard d2stack rdmInput
  case move of
    mv@(Draw drawStrat) -> if draw2stack game > 0
      then return (game
          { drawPile      = drop (2 * draw2stack game) $ drawPile game
          , hands         = addCards player (hands game) $ take (2 * draw2stack game) $ drawPile game
          , currentPlayer = nextPlayerWithDelta 1 (length $ hands game) player (direction game)
          , draw2stack    = 0
          }, D None)
      else evaluateDrawStrategy False game drawStrat (seed + 1)
    mv@(Play card) -> if isValidMove game mv
      then return (playMove game mv, M card)
      else return (game, Err $ "The move \"" ++ show mv ++ "\" with game state: \n" ++ prettyShowGame game ++ " is invalid")
evaluateStrategy True seed game@Game { currentPlayer = player } strat = do
  let players  = deckSizes game
      hand     = hands game !! currentPlayer game
      lastCard = head $ discardPile game
      d2stack  = draw2stack game
      rdmInput = randoms $ mkStdGen seed
  move <- timeout timeLimit $ return $! strat players hand lastCard d2stack rdmInput
  case move of
    Nothing               -> return (game, Err "The strategy times out")
    Just (Draw drawStrat) -> if draw2stack game > 0
      then return (game
          { drawPile      = drop (2 * draw2stack game) $ drawPile game
          , hands         = addCards player (hands game) $ take (2 * draw2stack game) $ drawPile game
          , currentPlayer = nextPlayerWithDelta 1 (length $ hands game) player (direction game)
          , draw2stack    = 0
          }, D None)
      else evaluateDrawStrategy True game drawStrat (seed + 1)
    Just mv@(Play card) -> if isValidMove game mv
      then return (playMove game mv, M card)
      else return (game, Err $ "The move \"" ++ show mv ++ "\" with game state: \n" ++ prettyShowGame game ++ " is invalid")

evaluateDrawStrategy :: Bool -> Game -> DrawStrategy -> Seed -> IO (Game, Evaluation)
evaluateDrawStrategy True game strat seed = do
  let player   = currentPlayer game
      newCard  = head $ drawPile game
      newHands = addCard player (hands game) newCard
      newGame  = game { drawPile = tail $ drawPile game, hands = newHands }
      hand     = newHands !! player
      lastCard = head $ discardPile game
      players  = deckSizes newGame
      rdmInput = randoms $ mkStdGen seed
  move <- timeout timeLimit $ return $! strat players hand lastCard rdmInput
  case move of
    Nothing            -> return (game, Err "The draw strategy times out")
    Just None          -> return (performDrawMove newGame None, D None)
    Just mv@(Return _) -> if isValidDrawMove newCard newGame mv
      then return (performDrawMove newGame mv, D mv)
      else return (game, Err $ "The draw move \"" ++ show mv ++ "\" with game state: \n" ++ prettyShowGame game ++ " is invalid")
evaluateDrawStrategy False game strat seed = do
  let player   = currentPlayer game
      newCard  = head $ drawPile game
      newHands = addCard player (hands game) newCard
      newGame  = game { drawPile = tail $ drawPile game, hands = newHands }
      hand     = newHands !! player
      lastCard = head $ discardPile game
      players  = deckSizes newGame
      rdmInput = randoms $ mkStdGen seed
  move <- return $! strat players hand lastCard rdmInput
  case move of
    None          -> return (performDrawMove newGame None, D None)
    mv@(Return _) -> if isValidDrawMove newCard newGame mv
      then return (performDrawMove newGame mv, D mv)
      else return (game, Err $ "The draw move \"" ++ show mv ++ "\" with game state: \n" ++ prettyShowGame game ++ " is invalid")

-- perform the draw move on the game,
-- assumes that the draw move is checked as valid
performDrawMove :: Game -> DrawMove -> Game
performDrawMove game None = game { currentPlayer = nextPlayerWithDelta 1 (length $ hands game) (currentPlayer game) (direction game) }
performDrawMove game (Return card) = playMove game (Play card)

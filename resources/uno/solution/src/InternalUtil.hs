module InternalUtil where

import Util
import Types
import Data.List
import Prelude hiding (Left, Right)
import Exercise
import Data.Array.ST.Safe
import Control.Monad.ST
import Data.STRef
import Control.Monad
import System.Random
import Data.Maybe

{-
strategy for test purposes: you can run a game using 'playAndPrint' from GamePlay
or use 'playStrategies' from GamePlay to analyze the performance of your AI against 
other strategies (e.g. this strategy).

simpleStrategy plays the first valid card in hand or draws and does nothing
simpleStrategy uses isValidSuccessor from your implementation, therefore it doesn't work
if you haven't implemented the method yet.
-}
simpleStrategy :: Strategy
simpleStrategy _ hand lastCard stack _
  | stack /= 0 || null valid = Draw $ const $ const $ const $ const None
  | all isWildCard valid = case head valid of
      WildCard Wild _   -> Play $ WildCard Wild $ Just $ getColor lastCard
      WildCard Draw4 _  -> Play $ WildCard Draw4 $ Just $ getColor lastCard
  | otherwise = Play (head $ filter (not . isWildCard) valid)
  where
    valid = filter (\c -> isValidSuccessor lastCard c || isWildCard c) hand

    isWildCard :: Card -> Bool
    isWildCard (WildCard _ _) = True
    isWildCard _ = False

-- returns the deck sizes of all players except the current player in play direction
deckSizes :: Game -> [Int]
deckSizes Game {hands = hands, currentPlayer = currentPlayer, direction = Right} =
  map length $ tail $ rotate currentPlayer hands
  where
    rotate _ [] = []
    rotate n xs = zipWith const (drop n (cycle xs)) xs
deckSizes Game {hands = hands, currentPlayer = currentPlayer, direction = Left} =
  map length $ init $ reverse $ rotate currentPlayer hands
  where
    rotate _ [] = []
    rotate n xs = zipWith const (drop n (cycle xs)) xs

{-- startWithSeed --}

-- starts a game with given seed
startWithSeed :: Seed -> Int -> Game
startWithSeed seed numberOfPlayers = if isNumCard lastCard
  then Game drawPile [lastCard] hands 0 Right 0
  else Game newPile [newCard] hands 0 Right 0
 where
  cards                              = shuffleWithSeed seed deck
  (lastCard : drawPile, hands      ) = deal numberOfPlayers cards
  (a, newCard : b) = splitAt (fromJust $ findIndex isNumCard drawPile) drawPile
  newPile                            = a ++ b
  
  isNumCard :: Card -> Bool
  isNumCard (NumCard _ _) = True
  isNumCard _ = False

-- deals 7 cards to each player, 1 card at a time
deal :: Int -> [Card] -> ([Card], Hands)
deal numberOfPlayers cards
  | numberOfPlayers < 2  = error "A minimum of two players is required"
  | numberOfPlayers > 10 = error "A maximum of ten players is allowed"
  | otherwise            = aux 7 (numberOfPlayers - 1) (cards, replicate numberOfPlayers [])
 where
  aux :: Int -> Int -> ([Card], Hands) -> ([Card], Hands)
  aux 0 _ res = res
  aux _ _ ([], _) = error "There are no more cards to deal (this should never happen)"
  aux cardNumber 0 (card : cards, hands)            = aux (cardNumber - 1) (numberOfPlayers - 1) (cards, addCard 0 hands card)
  aux cardNumber playerNumber (card : cards, hands) = aux cardNumber (playerNumber - 1) (cards, addCard playerNumber hands card)

-- a list of all uno cards
deck :: [Card]
deck =
  [f i | f <- map NumCard colors, i <- [0 .. 9] ++ [1 .. 9]]
    ++ concatMap (replicate 2) [f i | f <- map ActCard colors, i <- [Skip, Reverse, Draw2]]
    ++ replicate 4 (WildCard Wild Nothing)
    ++ replicate 4 (WildCard Draw4 Nothing)
  where
    colors = [R, G, B, Y]

-- shuffle a deck of cards with seed for reproducibility
shuffleWithSeed :: Seed -> [Card] -> [Card]
shuffleWithSeed seed xs =
  runST $
    do
      gen <- newSTRef $ mkStdGen seed
      let n = length xs
          randomRST range = do
            (a, gen') <- randomR range <$> readSTRef gen
            -- write to stdgen every time to make it random enough
            writeSTRef gen gen'
            return a
      arr <- newListArray (1, n) xs :: ST s (STArray s Int Card)
      forM [1 .. n] $ \i -> do
        j <- randomRST (i, n)
        vi <- readArray arr i
        vj <- readArray arr j
        writeArray arr j vi
        return vj

{-- internal data types --}

-- winner, game statistics, reason
data Result = Result (Maybe Int) [(Game, Evaluation)] String

instance Semigroup Result where
  Result _ xs a <> Result i ys b = Result i (xs ++ ys) (a ++ b)

instance Monoid Result where
  mempty = Result Nothing [] ""

data Evaluation = M Card | D DrawMove | Err String

instance Show Evaluation where
  show (M   card         ) = "plays " ++ show card
  show (D   (Return card)) = "draws a card and plays " ++ show card
  show (D   None         ) = "draws a card and decides not to play"
  show (Err str          ) = "***" ++ str

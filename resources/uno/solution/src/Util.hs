module Util where

import Types
import Data.List
import Prelude hiding (Left, Right)

-- adds a card to the hand of a given player
addCard :: Int -> Hands -> Card -> Hands
addCard playerNumber hands card = xs ++ (card : player) : ys
  where (xs, player : ys) = splitAt playerNumber hands

-- adds cards to the hand of a given player
addCards :: Int -> Hands -> [Card] -> Hands
addCards playerNumber hands cards = xs ++ (cards ++ player) : ys
  where (xs, player : ys) = splitAt playerNumber hands

-- removes a card from the hand of a given player
removeCard :: Int -> Hands -> Card -> Hands
removeCard playerNumber hands card =
  xs ++ deleteBy compareCards card player : ys
 where
  (xs, player : ys) = splitAt playerNumber hands
  compareCards :: Card -> Card -> Bool
  compareCards (WildCard w _) (WildCard m _) = w == m
  compareCards a              b              = a == b

-- identifies the next player given a delta identifying how many players to move in a direction
nextPlayerWithDelta :: Int -> Int -> Int -> Direction -> Int
nextPlayerWithDelta delta numberOfPlayers currentPlayer Right =
  (currentPlayer + delta) `mod` numberOfPlayers
nextPlayerWithDelta delta numberOfPlayers currentPlayer Left =
  let i = (currentPlayer - delta) `mod` numberOfPlayers
  in  if i < 0 then numberOfPlayers - i else i

-- identifies the next player
-- lastCard -> numberOfPlayers -> currentPlayer -> direction -> nextPlayer
nextPlayer :: Card -> Int -> Int -> Direction -> Int
nextPlayer (ActCard  _     Skip) = nextPlayerWithDelta 2
nextPlayer (WildCard Draw4 _   ) = nextPlayerWithDelta 2
nextPlayer _                     = nextPlayerWithDelta 1

-- handle the special case of wild card's color when checking card in hand
hasCard :: Card -> Hand -> Bool
hasCard (WildCard Draw4 (Just _)) hand = WildCard Draw4 Nothing `elem` hand
hasCard (WildCard Wild  (Just _)) hand = WildCard Wild Nothing `elem` hand
hasCard card                      hand = card `elem` hand

-- returns color of card
-- ATTENTION -- 
-- throws undefined error if being called on a WildCard with no assigned color,
-- make sure to handle this case before calling this function where necessary
getColor :: Card -> Color
getColor (NumCard c _) = c
getColor (ActCard c _) = c
getColor (WildCard _ (Just c)) = c
getColor (WildCard _ Nothing) = undefined
module Exercise where

import Types
import Util

prettyShowGame :: Game -> String
prettyShowGame = undefined

isGameOver :: Game -> Bool
isGameOver = undefined

nextDirection :: Direction -> Card -> Direction
nextDirection = undefined

isValidSuccessor :: Card -> Card -> Bool
isValidSuccessor = undefined

isValidMove :: Game -> Move -> Bool
isValidMove = undefined

isValidDrawMove :: Card -> Game -> DrawMove -> Bool
isValidDrawMove = undefined

-- returns the next game state given a move
playMove :: Game -> Move -> Game
-- you only have to implement the handling of a played Draw2 card, the other cases are already implemented and do not have to be changed
playMove game@Game { hands = hands, currentPlayer = player, direction = direction } (Play card@(ActCard _ Draw2)) = undefined

playMove game@Game { hands = hands, currentPlayer = player, direction = direction, drawPile = drawPile } (Play card@(WildCard Draw4 _)) =
  let next = nextPlayerWithDelta 1 (length hands) player direction
  in  game
        { discardPile   = card : discardPile game
        , hands         = addCards next (removeCard player hands card) (take 4 drawPile)
        , drawPile      = drop 4 drawPile
        , currentPlayer = nextPlayer card (length hands) player direction
        }
playMove game@Game { hands = hands, currentPlayer = player, direction = direction } (Play card@(ActCard _ Reverse)) =
  let newDirection = nextDirection direction card
  in  game
        { discardPile   = card : discardPile game
        , hands         = removeCard player hands card
        , currentPlayer = if length hands == 2
                            then player
                            else nextPlayer card (length hands) player direction
        , direction     = newDirection
        }
playMove game@Game { hands = hands, currentPlayer = player, direction = direction } (Play card) = 
  game
    { discardPile   = card : discardPile game
    , hands         = removeCard player hands card
    , currentPlayer = nextPlayer card (length hands) player direction
    }
-- needs no implementation
playMove _ (Draw _) = undefined

unoAI :: Strategy
unoAI = undefined

unoDrawAI :: DrawStrategy
unoDrawAI = undefined

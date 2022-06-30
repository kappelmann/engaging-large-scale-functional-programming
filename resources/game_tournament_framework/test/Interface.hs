module Interface where

import qualified Exercise08 as E
import Types

hasWon :: Player -> Board -> Bool
hasWon = E.hasWon

canPlaceOrb :: Player -> Pos -> Board -> Bool
canPlaceOrb = E.canPlaceOrb

neighbors :: Size -> Pos -> [Pos]
neighbors = E.neighbors

updatePos :: (Int -> Int) -> Player -> Pos -> Board -> Board
updatePos = E.updatePos

putOrb :: Player -> Pos -> Board -> Board
putOrb = E.putOrb

ensureType :: StatefulStrategy a -> StatefulStrategy a
ensureType = id

strategyState = ensureType E.strategyState

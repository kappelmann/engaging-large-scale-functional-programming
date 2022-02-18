module Interface where

import qualified Exercise as E
import qualified Types as T
import Type
import Prelude hiding (Left, Right)

gameToGame :: Game -> T.Game
gameToGame (Game drawPile discardPile hands currentPlayer direction draw2stack) =
    T.Game (map card2Card drawPile) (map card2Card discardPile) (map (map card2Card) hands) currentPlayer (directionToDirection direction) draw2stack

directionToDirection :: Direction -> T.Direction
directionToDirection Left = T.Left
directionToDirection Right = T.Right

card2Card :: Card -> T.Card
card2Card (NumCard c i) = T.NumCard (colorToColor c) i
card2Card (ActCard c a) = T.ActCard (colorToColor c) (actionToAction a)
card2Card (WildCard w c) = T.WildCard (wild2Wild w) (mColorToMColor c)

colorToColor :: Color -> T.Color
colorToColor R = T.R
colorToColor G = T.G
colorToColor B = T.B
colorToColor Y = T.Y

mColorToMColor :: Maybe Color -> Maybe T.Color
mColorToMColor Nothing = Nothing
mColorToMColor (Just c) = Just $ colorToColor c

actionToAction :: Action -> T.Action
actionToAction Skip = T.Skip
actionToAction Reverse = T.Reverse
actionToAction Draw2 = T.Draw2

wild2Wild :: Wild -> T.Wild
wild2Wild Wild = T.Wild
wild2Wild Draw4 = T.Draw4

moveToMove :: Move -> T.Move
moveToMove (Play c) = T.Play $ card2Card c
moveToMove (Draw s) = T.Draw (\cards hand card rdms -> drawMoveToDrawMove $ s cards (map card2CardRev hand) (card2CardRev card) rdms)

drawMoveToDrawMove :: DrawMove -> T.DrawMove
drawMoveToDrawMove (Return c) = T.Return $ card2Card c
drawMoveToDrawMove None = T.None

card2CardRev :: T.Card -> Card
card2CardRev (T.NumCard c i) = NumCard (colorToColorRev c) i
card2CardRev (T.ActCard c a) = ActCard (colorToColorRev c) (actionToActionRev a)
card2CardRev (T.WildCard w c) = WildCard (wild2WildRev w) (mColorToMColorRev c)

colorToColorRev :: T.Color -> Color
colorToColorRev T.R = R
colorToColorRev T.G = G
colorToColorRev T.B = B
colorToColorRev T.Y = Y

mColorToMColorRev :: Maybe T.Color -> Maybe Color
mColorToMColorRev Nothing = Nothing
mColorToMColorRev (Just c) = Just $ colorToColorRev c

actionToActionRev :: T.Action -> Action
actionToActionRev T.Skip = Skip
actionToActionRev T.Reverse = Reverse
actionToActionRev T.Draw2 = Draw2

wild2WildRev :: T.Wild -> Wild
wild2WildRev T.Wild = Wild
wild2WildRev T.Draw4 = Draw4

directionToDirectionRev :: T.Direction -> Direction
directionToDirectionRev T.Left = Left
directionToDirectionRev T.Right = Right

gameToGameRev :: T.Game -> Game
gameToGameRev (T.Game drawPile discardPile hands currentPlayer direction draw2stack) =
    Game (map card2CardRev drawPile) (map card2CardRev discardPile) (map (map card2CardRev) hands) currentPlayer (directionToDirectionRev direction) draw2stack

moveToMoveRev :: T.Move -> Move
moveToMoveRev (T.Play c) = Play $ card2CardRev c
moveToMoveRev (T.Draw s) = Draw (\cards hand card rdms -> drawMoveToDrawMoveRev $ s cards (map card2Card hand) (card2Card card) rdms)

drawMoveToDrawMoveRev :: T.DrawMove -> DrawMove
drawMoveToDrawMoveRev (T.Return c) = Return $ card2CardRev c
drawMoveToDrawMoveRev T.None = None

{-functions that should be implemented-}

prettyShowGame :: Game -> String
prettyShowGame g = E.prettyShowGame $ gameToGame g

isGameOver :: Game -> Bool
isGameOver g = E.isGameOver $ gameToGame g

nextDirection :: Direction -> Card -> Direction
nextDirection dir c = directionToDirectionRev $ E.nextDirection (directionToDirection dir) (card2Card c)

isValidSuccessor :: Card -> Card -> Bool
isValidSuccessor c1 c2 = E.isValidSuccessor (card2Card c1) (card2Card c2)

isValidMove :: Game -> Move -> Bool
isValidMove g m = E.isValidMove (gameToGame g) (moveToMove m)

isValidDrawMove :: Card -> Game -> DrawMove -> Bool
isValidDrawMove c g m = E.isValidDrawMove (card2Card c) (gameToGame g) (drawMoveToDrawMove m)

playMove :: Game -> Move -> Game
playMove g m = gameToGameRev $ E.playMove (gameToGame g) (moveToMove m)

unoAI :: Strategy
unoAI cards hand card draw2stack ds = moveToMoveRev $ E.unoAI cards (map card2Card hand) (card2Card card) draw2stack ds

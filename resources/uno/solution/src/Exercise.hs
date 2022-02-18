module Exercise where

import Types
import Util
import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe

-- pretty prints game state
prettyShowGame :: Game -> String
prettyShowGame g =
  "Last Played Card: " ++ show (head $ discardPile g)
    ++ "\nDirection: "
    ++ prettyShowDirection (direction g)
    ++ "\nDraw2Stack: "
    ++ show (draw2stack g)
    ++ "\n---\n"
    ++ prettyShowPlayerHands g
    ++ "\n"
  where
    prettyShowDirection :: Direction -> String
    prettyShowDirection Left = "<-"
    prettyShowDirection Right = "->"

prettyShowPlayerHands :: Game -> String
prettyShowPlayerHands g = intercalate "\n" (zipWith (curry prettyShowPlayer) [0 ..] (hands g))
  where
    prettyShowPlayer :: (Int, Hand) -> String
    prettyShowPlayer (i, hand) = prettyShowCurrentPlayer i ++ " Player " ++ show i ++ ": " ++ prettyShowHand hand
    prettyShowCurrentPlayer :: Int -> String
    prettyShowCurrentPlayer i = if currentPlayer g == i then ">" else " "
    prettyShowHand :: Hand -> String
    prettyShowHand h = "[ " ++ intercalate ", " (map show h) ++ " ]"

-- checks if anyone has their hand empty which wins the game
isGameOver :: Game -> Bool
isGameOver game = any null $ hands game

-- identifies the next direction
nextDirection :: Direction -> Card -> Direction
nextDirection Left      (ActCard _ Reverse) = Right
nextDirection Right     (ActCard _ Reverse) = Left
nextDirection direction _                   = direction

-- checks if the card passed as second argument is a valid successor of the card passed as first argument
isValidSuccessor :: Card -> Card -> Bool
isValidSuccessor (NumCard colorLast intLast)    (NumCard colorPlayer intPlayer)    = colorLast == colorPlayer || intLast == intPlayer
isValidSuccessor (NumCard colorLast _)          (ActCard colorPlayer _)            = colorLast == colorPlayer
isValidSuccessor (ActCard colorLast _)          (NumCard colorPlayer _)            = colorLast == colorPlayer
isValidSuccessor (ActCard colorLast actionLast) (ActCard colorPlayer actionPlayer) = colorLast == colorPlayer || actionLast == actionPlayer
isValidSuccessor (WildCard _ (Just colorLast))  (NumCard colorPlayer _)            = colorLast == colorPlayer
isValidSuccessor (WildCard _ (Just colorLast))  (ActCard colorPlayer _)            = colorLast == colorPlayer
isValidSuccessor _ (WildCard _ Nothing) = False
isValidSuccessor (WildCard _ Nothing) _ = True
isValidSuccessor _ (WildCard _ _)       = True

isDraw2 :: Card -> Bool
isDraw2 (ActCard _ Draw2) = True
isDraw2 _ = False

-- checks if a move is valid in a given game state
-- under assumption that discardPile is not empty
isValidMove :: Game -> Move -> Bool
isValidMove (Game _ discardPile hands currentPlayer _ draw2stack) (Play card)
  | draw2stack > 0 =
      hasCard card (hands !! currentPlayer) && isDraw2 card
  | otherwise =
      hasCard card (hands !! currentPlayer) && isValidSuccessor (head discardPile) card
isValidMove game (Draw _) = not $ null $ drawPile game

-- only the most recent drawn card can be played
isValidDrawMove :: Card -> Game -> DrawMove -> Bool
isValidDrawMove drawnCard game (Return card) = compareCards drawnCard card && isValidMove game (Play card)
  where
    compareCards :: Card -> Card -> Bool
    compareCards (WildCard w _) (WildCard m _) = w == m
    compareCards a              b              = a == b
isValidDrawMove _ _ None = True

-- returns the next game state given a move
playMove :: Game -> Move -> Game
playMove game@Game { hands = hands, currentPlayer = player, direction = direction } (Play card@(ActCard _ Draw2)) =
  game
    { discardPile   = card : discardPile game
    , hands         = removeCard player hands card
    , currentPlayer = nextPlayer card (length hands) player direction
    , draw2stack    = draw2stack game + 1
    }
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

isSomeWild :: Card -> Bool
isSomeWild (WildCard _ _) = True
isSomeWild _ = False

-- example strategy
unoAI :: Strategy
unoAI others hand lastCard@(WildCard _ Nothing) _ _
  | null valid = Draw unoDrawAI
  | otherwise = Play bestChoice
  where
    valid = filter (\c -> isValidSuccessor lastCard c || isSomeWild c) hand
    prefs = getColorPreferences hand
    bestChoice = maximumBy (cardCompare (head others) prefs) (putMajorityWild (head prefs) valid)
unoAI others hand lastCard draw2stack _
  | draw2stack /= 0 =
    let draw2s = filter isDraw2 hand
     in if null draw2s
          then Draw unoDrawAI
          else Play $ twoStrat draw2s
  | null valids = Draw unoDrawAI
  | otherwise = Play bestChoice
  where
    valids = filter (\c -> isValidSuccessor lastCard c || isSomeWild c) hand
    prefs = getColorPreferences hand
    bestChoice = maximumBy (cardCompare (head others) prefs) (putMajorityWild (head prefs) valids)
    twoStrat xs = maximumBy (sortByColor prefs) xs

unoDrawAI :: DrawStrategy
unoDrawAI _ hand lastCard _
  | isSomeWild (head hand) = Return $ head $ putMajorityWild (getBestColor hand) hand
  | isValidSuccessor lastCard (head hand) = Return $ head hand
  | otherwise = None

cardCompare :: Int -> [Color] -> Card -> Card -> Ordering
cardCompare opp cs x y
  | opp > 1 && cardCompareLoose x y /= EQ = cardCompareLoose x y
  | opp <= 1 && cardCompareHard x y /= EQ = cardCompareHard x y
  | otherwise = sortByColor cs y x

cardCompareLoose :: Card -> Card -> Ordering
cardCompareLoose (WildCard Wild _) (WildCard Draw4 _) = GT
cardCompareLoose (WildCard Draw4 _) (WildCard Wild _) = LT
cardCompareLoose (WildCard _ _) _ = LT
cardCompareLoose _ (WildCard _ _) = GT
cardCompareLoose (NumCard _ _) (ActCard _ Draw2) = GT
cardCompareLoose (ActCard _ Draw2) (NumCard _ _) = LT
cardCompareLoose (NumCard _ _) (ActCard _ _) = LT
cardCompareLoose (ActCard _ _) (NumCard _ _) = GT
cardCompareLoose _ _ = EQ

cardCompareHard :: Card -> Card -> Ordering
cardCompareHard (WildCard Draw4 _) _ = GT
cardCompareHard _ (WildCard Draw4 _) = LT
cardCompareHard (WildCard Wild _) _ = LT
cardCompareHard _ (WildCard Wild _) = GT
cardCompareHard _ (ActCard _ Draw2) = LT
cardCompareHard (ActCard _ Draw2) _ = GT
cardCompareHard (NumCard _ _) (ActCard _ _) = LT
cardCompareHard (ActCard _ _) (NumCard _ _) = GT
cardCompareHard _ _ = EQ

sortByColor :: [Color] -> Card -> Card -> Ordering
sortByColor cs x y = compare a b
  where
    a = fromJust $ elemIndex (getColor x) cs
    b = fromJust $ elemIndex (getColor y) cs

getBestColor :: Hand -> Color
getBestColor = head . getColorPreferences

getColorPreferences :: Hand -> [Color]
getColorPreferences hand = aux hand $ zip [R, G, B, Y] $ repeat 0
  where
    aux [] res = map fst $ sortBy (flip (\(_, a) (_, b) -> compare a b)) res
    aux (WildCard _ _ : cs) res = aux cs res
    aux (c : cs) res = aux cs $ map (\(co, n) -> if co == getColor c then (co, n + 1) else (co, n)) res

putMajorityWild :: Color -> Hand -> Hand
putMajorityWild _ [] = []
putMajorityWild c (WildCard Wild _ : xs) = WildCard Wild (Just c) : putMajorityWild c xs
putMajorityWild c (WildCard Draw4 _ : xs) = WildCard Draw4 (Just c) : putMajorityWild c xs
putMajorityWild c (card : xs) = card : putMajorityWild c xs

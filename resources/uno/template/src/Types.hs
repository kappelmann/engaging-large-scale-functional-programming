module Types where

{-predefined data types-}

-- different card types
data Card = NumCard Color Int | ActCard Color Action | WildCard Wild (Maybe Color)
  deriving Eq

-- for your convenience, we included an instantiation of Show for Card
instance Show Card where
  show (NumCard  c i) = show c ++ " " ++ show i
  show (ActCard  c a) = show c ++ " " ++ show a
  show (WildCard w c) = show w ++ maybe "" (mappend " " . show) c

-- Colors: Red, Green, Blue, Yellow
data Color = R | G | B | Y
  deriving (Show, Eq, Read)

-- possible types of Action cards
data Action = Skip | Reverse | Draw2
  deriving (Show, Eq, Read)

-- possible types of Wild cards
data Wild = Wild | Draw4
  deriving (Show, Eq, Read)

-- a pile of cards
type Pile = [Card]

-- a hand is a list of cards
type Hand = [Card]

-- hands of different players
type Hands = [Hand]

-- the play direction is either left (-) or right (+)
data Direction = Left | Right
  deriving (Show, Eq)

-- a game state consists of a draw and a discard pile, the players' hands,
-- the current player, the current game direction and if applicable, the number of stacked +2 cards
-- we are using records here, for more information, see [http://learnyouahaskell.com/making-our-own-types-and-typeclasses]
data Game = Game
  { drawPile :: Pile,
    discardPile :: Pile,
    hands :: Hands,
    currentPlayer :: Int,
    direction :: Direction,
    draw2stack :: Int
  }
  deriving (Show, Eq)

-- a move is either playing a card or drawing a card
-- if a card is drawn, the next move is given by DrawStrategy
data Move = Play Card | Draw DrawStrategy

instance Show Move where
  show (Play uno) = "plays " ++ show uno
  show (Draw _  ) = "draws card"

-- after a card is drawn, you can either return the card that you have drawn or do nothing
data DrawMove = Return Card | None
  deriving (Show, Eq)

-- strategy gets
-- 1. list of numbers of cards of the next players in play direction
-- 2. own hand
-- 3. last played card
-- 4. draw2stack from Game state
-- 5. list of random Double values
type Strategy = [Int] -> Hand -> Card -> Int -> [Double] -> Move

-- draw strategy gets
-- 1. list of numbers of cards of the next players in play direction
-- 2. own hand (the first one being the card just drawn)
-- 3. last played card
-- 4. list of random Double values
type DrawStrategy = [Int] -> Hand -> Card -> [Double] -> DrawMove

type Seed = Int

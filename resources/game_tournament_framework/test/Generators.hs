module Generators where

import qualified Solution as Sol

import Control.Monad
import Debug.Trace
import Test.QuickCheck.Assertions
import Test.Tasty.QuickCheck
import Types
import Util


newtype BoardState = BoardState Board deriving (Eq)
instance Show BoardState where
  show (BoardState b) = "Board: " ++ show b ++ "\n" ++ Sol.showBoard b
instance Arbitrary BoardState where
  arbitrary = do
    s@(y, x) <- unpair <$> getSize
    vals <- mapM validField $ enumFromTo 0 (y * x - 1)
    return $ BoardState $ foldr setPos (makeBoard s) vals


newtype PlayerGen = PlayerGen Player
instance Show PlayerGen where
  show (PlayerGen p) = "Player: " ++ show p
instance Arbitrary PlayerGen where
  arbitrary = PlayerGen <$> elements [-1, 1]


newtype GetSize = GetSize Size
instance Show GetSize where
  show (GetSize size) = "Size: " ++ show size
instance Arbitrary GetSize where
  arbitrary = GetSize . unpair <$> getSize


newtype ValidPos = ValidPos Pos
instance Show ValidPos where
  show (ValidPos pos) = "Pos: " ++ show pos
instance Arbitrary ValidPos where
  arbitrary = do
    (y, x) <- unpair <$> getSize
    y' <- choose (0, y - 1)
    x' <- choose (0, x - 1)
    return $ ValidPos (y', x')


data UpdateFunc = UpdateFunc (Int -> Int) String
instance Show UpdateFunc where
  show (UpdateFunc _ s) = "UpdateFunc: " ++ s
instance Arbitrary UpdateFunc where
  arbitrary = do
    (NonNegative (Small n)) <- arbitrary
    elements [
      UpdateFunc (* n) ("(* " ++ show n ++ ")"),
      UpdateFunc (+ n) ("(+ " ++ show n ++ ")"),
      UpdateFunc (abs . flip (-) n) ("(abs . flip (-) " ++ show n ++ ")"),
      UpdateFunc (const n) ("(const " ++ show n ++ ")")
      ]


newtype RandomsGen = RandomsGen Int
instance Show RandomsGen where
  show (RandomsGen n) = "Randoms: mkRandoms " ++ show n
instance Arbitrary RandomsGen where
  arbitrary = RandomsGen <$> choose (minBound, maxBound)

data PutOrbGen = PutOrbGen [[Int]] Pos Int

instance Show PutOrbGen where
  show (PutOrbGen b pos pl) = "Player: " ++ show pl ++ "\nPosition: " ++ show pos ++ "\n" ++ show (BoardState b)

instance Arbitrary PutOrbGen where
  arbitrary = do
    s@(r, c) <- unpair <$> getSize
    b <- sequence [sequence [choose $ range s (y, x) | x <- [0..c-1]] | y <- [0..r-1]]
    y <- choose (0, r-1)
    x <- choose (0, c-1)
    let pos = (y, x)
    let pl = signum $ Sol.getCell pos b
    return (PutOrbGen b pos pl)
      where
        range size pos = let n = criticalMass size pos in if n == 0 then (0, 0) else (-n + 1, n - 1)

data ReactGen = ReactGen Board Pos Player

instance Show ReactGen where
  show (ReactGen b pos pl) = "Player: " ++ show pl ++ "\nPosition: " ++ show pos ++ "\n" ++ show (BoardState b)

instance Arbitrary ReactGen where
  arbitrary = do
    s@(r, c) <- unpair <$> getSize

    -- generates a board
    b <- sequence [sequence [orbs s (y, x) | x <- [0..c-1]] | y <- [0..r-1]]

    -- find all the positions that cause a reaction
    let react = map fst $ filter (\(p, f) -> abs f == criticalMass s p - 1) $ fields b

    -- if there are no reaction then we generate a random position
    -- otherwise we choose a random position
    pos <- if null react then
        do
          y <- choose (0, r-1)
          x <- choose (0, c-1)
          return (y, x)
      else elements react
    -- which player needs to place the orb
    let pl = let p = signum $ Sol.getCell pos b in if p == 0 then 1 else p
    return (ReactGen b pos pl)
      where
        orbs size pos = let n = criticalMass size pos in
          frequency $ map (\f -> ((abs f + 1) * 2, return f)) [(-n + 1)..(n - 1)]

-- Generates a board and position where placing one orb will win the game
data WinGen = WinGen Board Pos Player

instance Show WinGen where
  show (WinGen b pos pl) = "Player: " ++ show pl ++ "\nPosition: " ++ show pos ++ "\n" ++ show (BoardState b)

instance Arbitrary WinGen where
  arbitrary = do
    -- Generate a board using ReactionGen
    (ReactGen b pos pl) <- arbitrary

    -- Trigger the reaction
    let r = Sol.putOrb pl pos b

    -- The fields that still contain orbs of the opponent will be inverted.
    let a = if null r then b else map (map (\(o, a) -> if signum a == (-pl) then -o else o)) $ map (uncurry zip) (zip b r)
    return (WinGen a pos pl)


validField :: Int -> Gen (Pos, Field)
validField i = do
  s@(_,x) <- unpair <$> getSize
  let p = (i `div` x, i `mod` x)
  player <- elements [-1, 1]
  value <- choose (0, criticalMass s p - 1)
  return (p, player * value)

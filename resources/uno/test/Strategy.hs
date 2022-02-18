module Strategy where

{-strategy (AI) of the game-}

import Type
import Prelude hiding (Left, Right)
import Util

import Interface

import Data.List
import Data.Maybe
import Data.Functor (($>))
import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)

{---------------------------------------------------test strategies--------------------------------------------------}
-- this strategy only draws card and hope that the opponent make some mistake
realSimpleStrategy :: Strategy
realSimpleStrategy _ _ _ _ _ = Draw $ const $ const $ const $ const None

-- this strategy plays invalid moves and let the opponent win
invalidStrategy :: Strategy
invalidStrategy _ _ _ _ _ = Play $ WildCard Wild $ Just R

-- this strategy timeouts and let the opponent win
{-# NOINLINE timeoutStrategy #-}
timeoutStrategy :: Strategy
timeoutStrategy = unsafePerformIO $ threadDelay (10^7) $> realSimpleStrategy

{---------------------------------------------------tStrategy---------------------------------------------------}

tStrategy :: Strategy
tStrategy others hand lastCard@(WildCard _ Nothing) _ _
  | null valid = Draw drawStrategy
  | otherwise = Play bestChoice
  where
    valid = filter (\c -> isValidSuccessor lastCard c || isSomeWild c) hand
    prefs = getColorPreferences hand
    bestChoice = maximumBy (cardCompare (head others)) (putMajorityWild (head prefs) valid)
tStrategy others hand lastCard draw2stack _
  | draw2stack /= 0 =
    let draw2s = filter isDraw2 hand
     in if null draw2s
          then Draw drawStrategy
          else Play $ head draw2s
  | null valid = Draw drawStrategy
  | otherwise = Play bestChoice
  where
    valid = filter (\c -> isValidSuccessor lastCard c || isSomeWild c) hand
    prefs = getColorPreferences hand
    bestChoice = maximumBy (cardCompare (head others)) (putMajorityWild (head prefs) valid)

cardCompare :: Int -> Card -> Card -> Ordering
cardCompare opp
  | opp > 3 = cardCompareLoose
  | otherwise = cardCompareHard

cardCompareLoose :: Card -> Card -> Ordering
cardCompareLoose (WildCard Wild _) (WildCard Draw4 _) = GT
cardCompareLoose (WildCard Draw4 _) (WildCard Wild _) = LT
cardCompareLoose (WildCard _ _) _ = LT
cardCompareLoose _ (WildCard _ _) = GT
cardCompareLoose (NumCard _ _) (ActCard _ _) = LT
cardCompareLoose (ActCard _ _) (NumCard _ _) = GT
cardCompareLoose _ _ = EQ

cardCompareHard :: Card -> Card -> Ordering
cardCompareHard (WildCard Draw4 _) _ = GT
cardCompareHard _ (WildCard Draw4 _) = LT
cardCompareHard (WildCard Wild _) _ = LT
cardCompareHard _ (WildCard Wild _) = GT
cardCompareHard (NumCard _ _) (ActCard _ _) = LT
cardCompareHard (ActCard _ _) (NumCard _ _) = GT
cardCompareHard _ _ = EQ

{---------------------------------------------------fStrategy---------------------------------------------------}

fStrategy :: Strategy
fStrategy others hand lastCard@(WildCard _ Nothing) _ _
  | null valid = Draw drawStrategy
  | otherwise = Play bestChoice
  where
    valid = filter (\c -> isValidSuccessor lastCard c || isSomeWild c) hand
    prefs = getColorPreferences hand
    bestChoice = maximumBy (cardCompareF (head others) prefs) (putMajorityWild (head prefs) valid)
fStrategy others hand lastCard draw2stack _
  | draw2stack /= 0 =
    let draw2s = filter isDraw2 hand
     in if null draw2s
          then Draw drawStrategy
          else Play $ twoStrat draw2s
  | null valids = Draw drawStrategy
  | otherwise = Play bestChoice
  where
    valids = filter (\c -> isValidSuccessor lastCard c || isSomeWild c) hand
    prefs = getColorPreferences hand
    bestChoice = maximumBy (cardCompareF (head others) prefs) (putMajorityWild (head prefs) valids)
    twoStrat xs = maximumBy (sortByColor prefs) xs

drawStrategy :: DrawStrategy
drawStrategy _ hand lastCard _
  | isSomeWild (head hand) = Return $ head $ putMajorityWild (getBestColor hand) hand
  | isValidSuccessor lastCard (head hand) = Return $ head hand
  | otherwise = None

cardCompareF :: Int -> [Color] -> Card -> Card -> Ordering
cardCompareF opp cs x y
  | opp > 1 && cardCompareLooseF x y /= EQ = cardCompareLooseF x y
  | opp <= 1 && cardCompareHardF x y /= EQ = cardCompareHardF x y
  | otherwise = sortByColor cs y x

cardCompareLooseF :: Card -> Card -> Ordering
cardCompareLooseF (WildCard Wild _) (WildCard Draw4 _) = GT
cardCompareLooseF (WildCard Draw4 _) (WildCard Wild _) = LT
cardCompareLooseF (WildCard _ _) _ = LT
cardCompareLooseF _ (WildCard _ _) = GT
cardCompareLooseF (NumCard _ _) (ActCard _ Draw2) = GT
cardCompareLooseF (ActCard _ Draw2) (NumCard _ _) = LT
cardCompareLooseF (NumCard _ _) (ActCard _ _) = LT
cardCompareLooseF (ActCard _ _) (NumCard _ _) = GT
cardCompareLooseF _ _ = EQ

cardCompareHardF :: Card -> Card -> Ordering
cardCompareHardF (WildCard Draw4 _) _ = GT
cardCompareHardF _ (WildCard Draw4 _) = LT
cardCompareHardF (WildCard Wild _) _ = LT
cardCompareHardF _ (WildCard Wild _) = GT
cardCompareHardF _ (ActCard _ Draw2) = LT
cardCompareHardF (ActCard _ Draw2) _ = GT
cardCompareHardF (NumCard _ _) (ActCard _ _) = LT
cardCompareHardF (ActCard _ _) (NumCard _ _) = GT
cardCompareHardF _ _ = EQ

{---------------------------------------------------helpers---------------------------------------------------}

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

isDraw4 :: Card -> Bool
isDraw4 (WildCard Draw4 _) = True
isDraw4 _ = False

isDraw2 :: Card -> Bool
isDraw2 (ActCard _ Draw2) = True
isDraw2 _ = False

isSomeWild :: Card -> Bool
isSomeWild (WildCard _ _) = True
isSomeWild _ = False

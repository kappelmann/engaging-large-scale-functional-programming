{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Test where

import qualified Interface as Sub
import qualified Solution  as Sol

import Control.Monad
import Debug.Trace
import Test.QuickCheck.Assertions
import Test.SmallCheck.Series     as SCS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck      as QC
import Test.Tasty.Runners.AntXML
import Test.Tasty.SmallCheck      as SC

import Control.Exception (evaluate, SomeException, try)
import Data.Ord (comparing)
import Data.List (maximumBy)
import System.Environment (setEnv)
import System.IO.Unsafe (unsafePerformIO)

import Generators
import Types
import Util

-- use quick check timeouts to ensure that in case of a timeout the offending input is printed
timeout = QC.within 100000


prop_canPlaceOrb (PlayerGen p) (ValidPos pos) (BoardState b) =
  timeout $
  QC.classify (Sol.canPlaceOrb p pos b) "True" $
    Sol.canPlaceOrb p pos b ==? Sub.canPlaceOrb p pos b

unit_canPlaceOrb :: [TestTree]
unit_canPlaceOrb = [
    t "Unit Tests/canPlaceOrb 1 (0,0) [[0,0],[0,0]]" 1 (0, 0) [[0,0],[0,0]],
    t "Unit Tests/canPlaceOrb (-1) (0,0) [[0,0],[0,0]]" (-1) (0, 0) [[0,0],[0,0]],
    t "Unit Tests/canPlaceOrb 1 (0,0) [[1,0],[0,0]]" 1 (0, 0) [[1,0],[0,0]],
    t "Unit Tests/canPlaceOrb (-1) (0,0) [[-1,0],[0,0]]" (-1) (0, 0) [[-1,0],[0,0]],
    t "Unit Tests/canPlaceOrb 1 (1,1) [[0,0,0],[0,2,0],[0,0,0]]" 1 (1, 1) [[0,0,0],[0,2,0],[0,0,0]],
    t "Unit Tests/canPlaceOrb (-1) (1,1) [[0,0,0],[0,-2,0],[0,0,0]]" (-1) (1, 1) [[0,0,0],[0,-2,0],[0,0,0]],
    t "Unit Tests/canPlaceOrb 1 (0,0) [[-1,0],[0,0]]" 1 (0, 0) [[-1,0],[0,0]],
    t "Unit Tests/canPlaceOrb (-1) (0,0) [[1,0],[0,0]]" (-1) (0, 0) [[1,0],[0,0]]
  ]
    where
      t name player pos board = testCase name (Sol.canPlaceOrb player pos board @=? Sub.canPlaceOrb player pos board)

-- about 1% of generated boards wins, so just skip them and generate winning boards instead
prop_hasWonFalse (PlayerGen p) (BoardState b) =
  timeout $
  length (filter (/= 0) $ concat b) > 1 QC.==>
  checkWon b == 0 QC.==>
    False ==? Sub.hasWon p b

prop_hasWonTrue (PlayerGen p) (BoardState b) =
  timeout $
  length (filter (/= 0) $ concat b) > 1 QC.==>
    True ==? Sub.hasWon p (map (map ((p*) . abs)) b)

-- Unit tests for hasWon.
unit_hasWon :: [TestTree]
unit_hasWon = [
    t "Unit Tests/hasWon 1 [[1,1],[0,0]]" 1 [[1,1],[0,0]],
    t "Unit Tests/hasWon (-1) [[-1,-1],[0,0]]" (-1) [[-1,-1],[0,0]],
    t "Unit Tests/hasWon 1 [[0,0],[1,1]]" 1 [[0,0],[1,1]],
    t "Unit Tests/hasWon (-1) [[0,0],[-1,-1]]" (-1) [[0,0],[-1,-1]],
    t "Unit Tests/hasWon 1 [[1,0],[-1,0]]" 1 [[1,0],[-1,0]],
    t "Unit Tests/hasWon (-1) [[-1,0],[1,0]]" (-1) [[-1,0],[1,0]]
  ]
    where
      t name p b = testCase name (Sol.hasWon p b @=? Sub.hasWon p b)

prop_neighbors (GetSize s) (ValidPos p) =
  timeout $
  QC.label ("Number of neighbors: " ++ show (length sol)) $
    SetEq sol ==? SetEq (Sub.neighbors s p)
  where
    sol = Sol.neighbors s p

-- Unit tests for neighbors.
unit_neighbors :: [TestTree]
unit_neighbors = [
    t "Unit Tests/neighbors (2,2) (0,0)" (2, 2) (0, 0),
    t "Unit Tests/neighbors (2,2) (0,1)" (2, 2) (0, 1),
    t "Unit Tests/neighbors (2,2) (1,0)" (2, 2) (1, 0),
    t "Unit Tests/neighbors (2,2) (1,1)" (2, 2) (1, 1),
    t "Unit Tests/neighbors (3,3) (0,0)" (3, 3) (0, 0),
    t "Unit Tests/neighbors (3,3) (0,1)" (3, 3) (0, 1),
    t "Unit Tests/neighbors (3,3) (0,2)" (3, 3) (0, 2),
    t "Unit Tests/neighbors (3,3) (1,0)" (3, 3) (1, 0),
    t "Unit Tests/neighbors (3,3) (1,1)" (3, 3) (1, 1),
    t "Unit Tests/neighbors (3,3) (1,2)" (3, 3) (1, 2),
    t "Unit Tests/neighbors (3,3) (2,0)" (3, 3) (2, 0),
    t "Unit Tests/neighbors (3,3) (2,1)" (3, 3) (2, 1),
    t "Unit Tests/neighbors (3,3) (2,2)" (3, 3) (2, 2)
  ]
    where
      t name size pos = testCase name $ SetEq (Sol.neighbors size pos) @=? SetEq (Sub.neighbors size pos)

prop_updatePos (UpdateFunc f _) (PlayerGen p) (ValidPos pos) (BoardState b) =
  timeout $ QC.property $
  BoardState (Sol.updatePos f p pos b) ==? BoardState (Sub.updatePos f p pos b)

unit_updatePos :: [TestTree]
unit_updatePos = [
    t "Unit Tests/updatePos (1+) 1 (0,0) [[0,0],[0,0]]" (1+) 1 (0, 0) [[0,0],[0,0]],
    t "Unit Tests/updatePos (1+) -1 (0,0) [[0,0],[0,0]]" (1+) (-1) (0, 0) [[0,0],[0,0]],
    t "Unit Tests/updatePos (1+) 1 (0,0) [[1,0],[0,0]]" (1+) 1 (0, 0) [[1,0],[0,0]],
    t "Unit Tests/updatePos (1+) -1 (0,0) [[1,0],[0,0]]" (1+) (-1) (0, 0) [[1,0],[0,0]],
    t "Unit Tests/updatePos (1+) 1 (0,0) [[-1,0],[0,0]]" (1+) 1 (0, 0) [[-1,0],[0,0]],
    t "Unit Tests/updatePos (1+) -1 (0,0) [[-1,0],[0,0]]" (1+) (-1) (0, 0) [[-1,0],[0,0]]
  ]
    where
      t name f pl pos b = testCase name (Sol.updatePos f pl pos b @=? Sub.updatePos f pl pos b)

prop_putOrb (PutOrbGen b pos pl) = timeout $ pl /= 0 QC.==> if null sol then QC.counterexample "Win not detected" $ Sub.hasWon pl sub else QC.property $ sub ?== sol
  where
    sol = Sol.putOrb pl pos b
    sub = Sub.putOrb pl pos b

prop_react (ReactGen b pos pl) = timeout $ pl /= 0 QC.==> if null sol then QC.counterexample "Win not detected" $ Sub.hasWon pl sub else QC.property $ sub ?== sol
  where
    sol = Sol.putOrb pl pos b
    sub = Sub.putOrb pl pos b

prop_win (WinGen b pos pl) = timeout $ pl /= 0 QC.==> null (Sol.putOrb pl pos b) QC.==> QC.counterexample "Win not detected" $ Sub.hasWon pl (Sub.putOrb pl pos b)


unit_putOrb :: [TestTree]
unit_putOrb = [
    t "Unit Tests/putOrb 1 (0,0) [[0,0],[0,0]]" 1 (0, 0) [[0,0],[0,0]],
    t "Unit Tests/putOrb (-1) (0,0) [[0,0],[0,0]]" (-1) (0, 0) [[0,0],[0,0]],
    t "Unit Tests/putOrb 1 (0,0) [[1,0],[0,-1]]" 1 (0, 0) [[1,0],[0,-1]],
    t "Unit Tests/putOrb (-1) (0,0) [[-1,0],[0,1]]" (-1) (0, 0) [[-1,0],[0,1]],
    t "Unit Tests/putOrb 1 (1,1) [[0,0,0],[0,3,0],[0,0,-1]]" 1 (1, 1) [[0,0,0],[0,3,0],[0,0,-1]],
    t "Unit Tests/putOrb (-1) (1,1) [[0,0,0],[0,-3,0],[0,0,1]]" (-1) (1, 1) [[0,0,0],[0,-3,0],[0,0,1]],
    t "Unit Tests/putOrb 1 (1,1) [[-1,0],[0,0]]" 1 (1, 1) [[-1,0],[0,0]],
    t "Unit Tests/putOrb (-1) (1,1) [[1,0],[0,0]]" (-1) (1, 1) [[1,0],[0,0]]
  ]
    where
      t name pl pos b = testCase name (Sol.putOrb pl pos b @=? Sub.putOrb pl pos b)

-- Internal tests to verify that WinGen is working properly
-- prop_winGen (WinGen b pos pl) = null (Sol.putOrb pl pos b) QC.==> null $ Sol.putOrb pl pos b

unsafeTry :: a -> Either SomeException a
unsafeTry = unsafePerformIO . try . evaluate

force :: (Pos, a) -> (Pos, a)
force v@((a,b),c) = a `seq` b `seq` v

playGame :: Show a => Show b => StatefulStrategy a -> StatefulStrategy b -> RandomsGen -> Size -> Either Player String
playGame (isa, sa) (isb, sb) (RandomsGen rss) (r, c) = go (Sol.mkRandoms rss) isa sa isb sb 1 (replicate r (replicate c 0))
  where
    -- type signature is necessary, inferred type is wrong!
    go :: Show a => Show b => [Int] -> a -> StatefulStrategyFunc a -> b -> StatefulStrategyFunc b -> Player -> Board -> Either Player String
    go _        _   _  _   _  p [] = Left (-p)
    go (rs:rss) stc sc stn sn p b  = go' (unsafeTry . force $ sc stc (Sol.mkRandoms rs) p b) rs rss stc sc stn sn p b
    go' (Left e)         rs _   stc _  _   _  p b
      -- cannot properly check in base-4.13.0.0; Timeout is only exported in base-4.14.0.0
      | show e == "<<timeout>>" = Right $ "Timeout with inputs:\n"++ show (RandomsGen rs) ++"\nState: " ++ show stc ++ "\nPlayer: " ++ show p ++ "\n" ++ show (BoardState b) ++ "\nThis call may not be particularly slow, your strategy may just be slow in general"
      | otherwise               = Right $ "Exception " ++ show e ++ " with inputs:\n" ++ show (RandomsGen rs) ++ "\nState: " ++ show stc ++ "\nPlayer: " ++ show p ++ "\n" ++ show (BoardState b)
    go' (Right (m, st')) rs rss stc sc stn sn p b
      | valid     = go rss stn sn st' sc (-p) (Sol.putOrb p m b)
      | otherwise = Right $ "Invalid move " ++ show m ++ " with inputs:\n" ++ show (RandomsGen rs) ++ "\nState: " ++ show stc ++ "\nPlayer: " ++ show p ++ "\n" ++ show (BoardState b)
      where
        valid     = Sol.isValidPos (size b) m && Sol.canPlaceOrb p m b

easyStrat :: Strategy
easyStrat _ pl b = fst . head $ filter (\(_, f) -> signum f /= (-pl)) $ fields b



putOrbHard :: Player -> Pos -> Board -> Board
putOrbHard p pos b
  | null b                             = b
  | movesHappened && Sol.hasWon p b_placed = b_placed
  | cell + 1 < length nb               = b_placed
  | otherwise                          = b_reduced
  where
    movesHappened = any (/= 0) $ concat b
    b_placed = Sol.updatePos succ p pos b
    s = size b
    cell = abs $ Sol.getCell pos b
    nb = Sol.neighbors s pos
    b_exploded = Sol.updatePos (const 0) p pos b
    b_reduced = foldr (putOrbHard p) b_exploded nb

{- x.3 -}
ownsPos :: Board -> Pos -> Int
ownsPos b (r, c) = signum $ b !! r !! c

playMove b p pos = putOrbHard p pos b

hardStrat :: Strategy
hardStrat _ p b = fst $ bestMove p $ mapToSnd (minimax (maxRecDepth -1) heuristic (- p) . playMove b p) (possMoves p b) -- Create tuples of (move, rating), then choose best
  where
    allMoves = [(r, c) | r <- [0 .. Sol.height b -1], c <- [0 .. Sol.width b -1]]
    possMoves p b = filter (\pos -> ownsPos b pos * p >= 0) allMoves
    bestMove :: Player -> [(Pos, Integer)] -> (Pos, Integer)
    bestMove p (m : ms) = foldr (\new@(_, newRating) current@(_, currentRating) -> if playerOptFunc p newRating currentRating then new else current) m ms
    playerOptFunc p = if p == -1 then (<) else (>)
    mapToSnd f vs = zip vs $ map f vs
    -- minimax takes max recursion depth, heuristic for board state, current player and current board. Returns the rating of best move
    minimax :: Integer -> (Player -> Board -> Integer) -> Player -> Board -> Integer
    minimax d h p b
      | Sol.hasWon p b = toInteger p * maxReward
      | Sol.hasWon (- p) b = toInteger (- p) * maxReward
      | d == 0 = h p b
      | d > 0 = snd $ bestMove p $ mapToSnd (minimax (d -1) h (- p) . playMove b p) (possMoves p b)
    heuristic p b = toInteger $ p * (length (possMoves p b) - length (filter (neighborOverflow p b) allMoves)) -- Number of possible moves for player p on current board, minus own cells which can be overflown by neighbor next round
    neighborOverflow p b pos = any (\position -> let cellVal = Sol.getCell position b in (p * cellVal < 0) && abs cellVal == length (Sol.neighbors (size b) position) - 1) $ Sol.neighbors (size b) pos
    maxReward = 1000000
    maxRecDepth = 2


data SolState = SolState
instance Show SolState where
  show _ = "If you can read this message there is an issue with the test. Please message us about this."
wrapStrategy :: Strategy -> StatefulStrategy SolState
wrapStrategy strat = (SolState, \_ r p b -> (strat r p b, SolState))

prop_canPlay :: Show a => Show b => StatefulStrategy a -> StatefulStrategy b -> Player -> RandomsGen -> QC.Property
prop_canPlay sa sb p rs = within (60*10^6) $ case playGame sa sb rs (9, 6) of
  Left w -> QC.property (w == p)
  Right s -> QC.counterexample s False

instance {-# OVERLAPPABLE #-} Show a where
  show _ = "your state does not derive Show and cannot be displayed"


-- Final tests wrap up and main

tests :: TestTree
tests = testGroup "Tests" [
    testGroup "canPlaceOrb" [
      testGroup "canPlaceOrb/Unit Tests" unit_canPlaceOrb,
      localOption (QuickCheckTests 500) $ QC.testProperty "canPlaceOrb/random" prop_canPlaceOrb
    ],
    testGroup "hasWon" [
      testGroup "hasWon/Unit Tests" unit_hasWon,
      QC.testProperty "hasWon/False" prop_hasWonFalse,
      QC.testProperty "hasWon/True" prop_hasWonTrue
    ],
    testGroup "neighbors" [
      testGroup "neighbors/Unit Tests" unit_neighbors,
      QC.testProperty "neighbors/random" prop_neighbors
    ],
    testGroup "updatePos" [
      testGroup "updatePos/Unit Tests" unit_updatePos,
      QC.testProperty "updatePos/random" prop_updatePos
    ],
    testGroup "putOrb" [
      testGroup "putOrb/Unit Tests" unit_putOrb,
      QC.testProperty "putOrb/random" prop_putOrb,
      QC.testProperty "putOrb/reaction" prop_react,
      QC.testProperty "putOrb/win" prop_win
    ],
    -- timeout here should be higher than quickchecks to ensure we can catch it and display a friendly error
    localOption (QuickCheckTests 1) $ localOption (mkTimeout (70*10^6)) $ testGroup "strategy" [
      QC.testProperty "strategy/starting against grandpa" $ prop_canPlay Sub.strategyState (wrapStrategy easyStrat) 1,
      QC.testProperty "strategy/grandpa starting" $ prop_canPlay (wrapStrategy easyStrat) Sub.strategyState (-1),
      QC.testProperty "strategy/starting against sister" $ prop_canPlay Sub.strategyState (wrapStrategy hardStrat) 1,
      QC.testProperty "strategy/sister starting" $ prop_canPlay (wrapStrategy hardStrat) Sub.strategyState (-1)
    ]
  ]


main :: IO ()
main = do
  -- set the default output file path as expected by bamboo
  -- the path can be overwritten by passing --xml=<pathGoesHere>
  setEnv "TASTY_XML" resultsPath
  testRunner $ localOption timeoutOption tests
  where
    resultsPath = "test-reports/results.xml"
    -- run tests with terminal output
    -- by default, run for 1 second
    timeoutOption = mkTimeout (1 * 10^6)
#ifdef LOCAL
    testRunner = defaultMain
#else
    testRunner = defaultMainWithIngredients [antXMLRunner]
#endif


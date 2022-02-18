{-# LANGUAGE LambdaCase #-}
module Test where

import qualified Interface as Sub
import qualified Solution as Sol

import Type
import qualified Type as T (Direction(..))
import Util
import InternalUtil
import GamePlay
import Strategy

import Control.Monad
import Data.Functor
import Data.List

import Test.Tasty
import Test.Tasty.Runners.AntXML
import Test.SmallCheck.Series as SCS
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Assertions
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit

import System.Environment (setEnv)

{-------------------- Generators --------------------}

instance Arbitrary Card where
    arbitrary = genCard

genCard :: Gen Card
genCard = frequency [(2, genNumCard), (2, genActCard), (2, genWildCard), (1, genWildCardNothing)]

genNumber :: Gen Int
genNumber = choose (0,9)

genColor :: Gen Color
genColor = elements [R, G, B, Y]

genAction :: Gen Action
genAction = elements [Skip, Reverse, Draw2]

genWild :: Gen Wild
genWild = elements [Wild, Draw4]

genNumCard :: Gen Card
genNumCard = NumCard <$> genColor <*> genNumber

genActCard :: Gen Card
genActCard = ActCard <$> genColor <*> genAction

genWildCard :: Gen Card
genWildCard = WildCard <$> genWild <*> elements (map Just [R, G, B, Y])

genWildCardNothing :: Gen Card
genWildCardNothing = flip WildCard Nothing <$> genWild

instance Arbitrary Direction where
    arbitrary = genDirection

genDirection :: Gen Direction
genDirection = elements [T.Left, T.Right]

instance Arbitrary Move where
    arbitrary = genMove

genMove :: Gen Move
genMove = frequency [(4, Play <$> genCard),(1, return $ Draw $ const $ const $ const $ const None)]

instance Arbitrary DrawMove where
    arbitrary = genDrawMove

genDrawMove :: Gen DrawMove
genDrawMove = frequency [(3, Return <$> genCard), (1, return None)]

instance Arbitrary Game where
    arbitrary = oneof [genGame2, genGame4, genInGame]

-- generate a random game with 2 players, each has 7 cards
genGame2 :: Gen Game
genGame2 = do
    drawPile  <- vectorOf 10 genCard
    player1   <- vectorOf 7 genCard
    player2   <- vectorOf 7 genCard
    player    <- choose (0,1)
    direction <- genDirection
    draw2s    <- choose (0,3)
    disCard   <- genPile (draw2s > 0)
    return $ Game drawPile disCard [player1, player2] player direction draw2s

-- generate a random game with 4 players, each has 7 cards
genGame4 :: Gen Game
genGame4 = do
    drawPile  <- vectorOf 10 genCard
    player1   <- vectorOf 7 genCard
    player2   <- vectorOf 7 genCard
    player3   <- vectorOf 7 genCard
    player4   <- vectorOf 7 genCard
    player    <- choose (0,3)
    direction <- genDirection
    draw2s    <- choose (0,3)
    disCard   <- genPile (draw2s > 0)
    return $ Game drawPile disCard [player1, player2, player3, player4] player direction draw2s

genInGame :: Gen Game
genInGame = do
    drawPile <- genPile False
    direction <- genDirection
    draw2s <- oneof [return 0, choose (1,3)]
    noOfPlayers <- choose (2,6)
    cur <- choose (0, noOfPlayers - 1)
    cards <- vectorOf noOfPlayers genHand
    discardPile <- genPile (draw2s > 0)
    return $ Game drawPile discardPile cards cur direction draw2s

genHand :: Gen Hand
genHand = flip vectorOf genCard =<< choose (1,8)

genPile :: Bool -> Gen [Card]
genPile b = 
    if b
        then do
            xs <- shuffle deck
            x <- genDraw2Card
            return (x:xs)
        else shuffle deck

newtype Draw2Move = Draw2Move Move
    deriving Show

instance Arbitrary Draw2Move where
    arbitrary = Draw2Move <$> genDraw2Move

genDraw2Move :: Gen Move
genDraw2Move = Play <$> genDraw2Card

genDraw2Card :: Gen Card
genDraw2Card = ActCard <$> genColor <*> return Draw2

{-------------------- Properties --------------------}

prop_prettyShowGame_simple :: Game -> QC.Property
prop_prettyShowGame_simple game =
    Sub.prettyShowGame game === Sol.prettyShowGame game

prop_isGameOver_simple :: Game -> QC.Property
prop_isGameOver_simple game =
    Sub.isGameOver game === Sol.isGameOver game

prop_nextDirection_simple :: Direction -> Card -> QC.Property
prop_nextDirection_simple dir card =
    Sub.nextDirection dir card === Sol.nextDirection dir card

prop_isValidSuccessor_simple :: Card -> Card -> QC.Property
prop_isValidSuccessor_simple prev succ =
    Sub.isValidSuccessor prev succ === Sol.isValidSuccessor prev succ

prop_isValidMove_simple :: Game -> Move -> QC.Property
prop_isValidMove_simple game move =
    Sub.isValidMove game move === Sol.isValidMove game move

prop_isValidDrawMove_simple :: Card -> Game -> DrawMove -> QC.Property
prop_isValidDrawMove_simple card game dm =
    Sub.isValidDrawMove card game dm === Sol.isValidDrawMove card game dm

prop_playMove_simple :: Game -> Draw2Move -> QC.Property
prop_playMove_simple game (Draw2Move move) =
    Sub.playMove game move === Sol.playMove game move

qcProperties :: TestTree
qcProperties = testGroup "simple tests"
    [ QC.testProperty "Test prettyShowGame"   prop_prettyShowGame_simple
    , QC.testProperty "Test isGameOver"       prop_isGameOver_simple
    , QC.testProperty "Test nextDirection"    prop_nextDirection_simple
    , QC.testProperty "Test isValidSuccessor" prop_isValidSuccessor_simple
    , QC.testProperty "Test isValidMove"      prop_isValidMove_simple
    , QC.testProperty "Test isValidDrawMove"  prop_isValidDrawMove_simple
    , QC.testProperty "Test playMove"         prop_playMove_simple
    ]

{-------------------- Test for strategies --------------------}

-- test if the player plays invalid move
prop_playsValidMove :: Seed -> QC.Property
prop_playsValidMove seed = monadicIO $ run $ liftM2 (.&&.) -- player should win both of the games
    -- player as first
    (playWithSeed True seed [Sub.unoAI, realSimpleStrategy]
    <&> \case
        Result Nothing  _ _ -> property True -- allow tie
        Result (Just p) _ _ -> p === 0)
    -- player as second
    (playWithSeed True seed [realSimpleStrategy, Sub.unoAI]
    <&> \case
        Result Nothing  _ _ -> property True -- allow tie
        Result (Just p) _ _ -> p === 1)

-- how many times to retry the game
{-# INLINE retries #-}
retries :: Int
retries = 200

-- play 100 games for given input in [seed, seed + 99]
-- return the count each player wins
countWinner :: Seed -> [Strategy] -> IO [Int]
countWinner seed strats = map (pred . length) . group . sort . mappend [0 .. length strats - 1] <$> count 0
    where
        count :: Seed -> IO [Int]
        count s
            | s >= retries = mempty
            | otherwise = playWithSeed True (seed + s) strats
                >>= \case
                    Result Nothing  _ _ -> count (s + 1)
                    Result (Just p) _ _ -> (:) p <$> count (s + 1)

-- do test run between 2 players
testBetween2 :: Seed -> Strategy -> Strategy -> Float -> QC.Property
testBetween2 seed player opponent threshold = monadicIO $ run $ do 
    winner1 <- countWinner seed [player, opponent]
    winner2 <- countWinner seed [opponent, player]
    return $ (fromIntegral (head winner1 + last winner2) / fromIntegral (2 * retries)) ?>= threshold

-- player beats the simpleStrategy in 2 players game
prop_playerIsNotSimple2 :: Seed -> QC.Property
prop_playerIsNotSimple2 seed = testBetween2 seed Sub.unoAI simpleStrategy 0.6

-- player is strong enough to beat the good AI in 2 players' game
prop_playerIsStrong2 :: Seed -> QC.Property
prop_playerIsStrong2 seed = testBetween2 seed Sub.unoAI fStrategy 0.5

-- do test run amoung 4 players. the first strategy is the testee
testAmong4 :: Seed -> [Strategy] -> Float -> QC.Property
testAmong4 seed players threshold = monadicIO $ run $ do 
    let stratss = rotations players
    let playerIndex = rotations [True, False, False, False]
    let gameCount = length playerIndex
    winners <- mapM (countWinner seed) stratss
    let playerWin = sum $ map snd $ filter fst $ concat $ zipWith zip playerIndex winners
    return $ (fromIntegral playerWin / fromIntegral (retries * gameCount)) ?>= threshold
        where
            -- use rotations instead of permutations, the latter is too slow
            rotations :: [a] -> [[a]]
            rotations xs = map (flip (drop <> take) xs) [0 .. length xs - 1]

-- player beats the simpleStrategy in 4 players' game
prop_playerIsNotSimple4 :: Seed -> QC.Property
prop_playerIsNotSimple4 seed = testAmong4 seed (Sub.unoAI : replicate 3 simpleStrategy) 0.3

-- player is strong enough to beat the good AI in 4 players' game
-- TODO: decide which opponent to use
prop_playerIsStrong4 :: Seed -> QC.Property
prop_playerIsStrong4 seed = testAmong4 seed  (Sub.unoAI : replicate 3 fStrategy) 0.24
strategyTests :: TestTree
strategyTests = testGroup "strategyTests"
    [ QC.testProperty "Test player plays only valid moves"           $ once prop_playsValidMove
    , QC.testProperty "Test player beats simple AI in 2 players' game" $ once prop_playerIsNotSimple2
    , QC.testProperty "Test player beats simple AI in 4 players' game" $ once prop_playerIsNotSimple4
    , QC.testProperty "Test player beats good AI in 2 players' game" $ once prop_playerIsStrong2
    , QC.testProperty "Test player beats good AI in 4 players' game" $ once prop_playerIsStrong4
    ]

{-------------------- Unit Tests --------------------}

card1 = NumCard R 1
game1 = Game [] [ActCard R Draw2] [[card1]] 0 Type.Left 1
move1 = Play card1
drawMove1 = Return card1

card2 = ActCard R Draw2
game2 = Game [] [ActCard B Draw2] [[card2]] 0 Type.Left 1
move2 = Play card2
drawMove2 = Return card2

card3 = NumCard R 1
game3 = Game [] [NumCard R 1] [[NumCard B 1]] 0 Type.Left 0
move3 = Play card3
drawMove3 = Return card3

card4 = WildCard Draw4 (Just R)
game4 = Game [] [NumCard R 1] [[WildCard Draw4 Nothing, NumCard R 1]] 0 Type.Left 0
move4 = Play card4
drawMove4 = Return card4

card5 = WildCard Draw4 Nothing
game5 = Game [] [NumCard R 1] [[card5]] 0 Type.Left 0
move5 = Play card5
drawMove5 = Return card5

game6 = Game [] [WildCard Draw4 Nothing] [[NumCard R 1]] 0 Type.Left 0
move6 = Draw drawStrategy

card7 = NumCard R 1
game7 = Game [] [NumCard R 1] [[card7]] 0 Type.Left 0
drawMove7 = Return card7

card8 = WildCard Wild Nothing
game8 = Game [] [WildCard Wild (Just Y)] [[card8]] 0 Type.Left 0
drawMove8 = Return (WildCard Wild (Just G))

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
    testGroup "Tests on nextDirection" [
        testCase "NumCard"  $ Type.Left @=? Sub.nextDirection Type.Left (NumCard B 1),
        testCase "WildCard" $ Type.Left @=? Sub.nextDirection Type.Left (WildCard Draw4 (Just R)),
        testCase "Skip"     $ Type.Left @=? Sub.nextDirection Type.Left (ActCard R Skip),
        testCase "Draw2"    $ Type.Left @=? Sub.nextDirection Type.Left (ActCard B Draw2),
        testCase "Reverse"  $ Type.Right @=? Sub.nextDirection Type.Left (ActCard B Reverse)
    ],
    testGroup "Tests on isValidSuccessor" [
        testCase "NumCard same color" $ True @=? Sub.isValidSuccessor (NumCard R 1) (NumCard R 2),
        testCase "NumCard same number" $ True @=? Sub.isValidSuccessor (NumCard R 1) (NumCard B 1),
        testCase "NumCard invalid" $ False @=? Sub.isValidSuccessor (NumCard R 1) (NumCard B 2),
        testCase "ActCard same color" $ True @=? Sub.isValidSuccessor (ActCard R Draw2) (ActCard R Skip),
        testCase "ActCard same action" $ True @=? Sub.isValidSuccessor (ActCard R Draw2) (ActCard B Draw2),
        testCase "ActCard invalid" $ False @=? Sub.isValidSuccessor (ActCard R Draw2) (ActCard B Skip),
        testCase "Card on empty wildcard (1)" $ True @=? Sub.isValidSuccessor (WildCard Wild Nothing) (NumCard R 1),
        testCase "Card on empty wildcard (2)" $ False @=? Sub.isValidSuccessor (WildCard Wild Nothing) (WildCard Wild Nothing),
        testCase "Card on Wildcard" $ True @=? Sub.isValidSuccessor (WildCard Wild (Just R)) (NumCard R 1),
        testCase "Empty WildCard on card" $ False @=? Sub.isValidSuccessor (NumCard R 1) (WildCard Wild Nothing)
    ],
    testGroup "Tests on isValidMove" [
        testCase "handles DrawStack (1)" $ False @=? Sub.isValidMove game1 move1,
        testCase "handles DrawStack (2)" $ True @=? Sub.isValidMove game2 move2,
        testCase "checks if player has Card" $ False @=? Sub.isValidMove game3 move3,
        testCase "checks if move is valid Draw4" $ True @=? Sub.isValidMove game4 move4,
        testCase "checks if player can play Empty Wildcard" $ False @=? Sub.isValidMove game5 move5,
        testCase "checks if draw pile is empty before draw move" $ False @=? Sub.isValidMove game6 move6],
    testGroup "Tests on isValidDrawMove" [
        testCase "handles DrawStack (1)" $ False @=? Sub.isValidDrawMove card1 game1 drawMove1,
        testCase "handles DrawStack (2)" $ True @=? Sub.isValidDrawMove card2 game2 drawMove2,
        testCase "checks if player has Card" $ False @=? Sub.isValidDrawMove card3 game3 drawMove3,
        testCase "checks if move is valid Draw4" $ True @=? Sub.isValidDrawMove card4 game4 drawMove4,
        testCase "checks if player can play Empty Wildcard" $ False @=? Sub.isValidDrawMove card5 game5 drawMove5,
        testCase "checks if played card is drawn card (1)" $ False @=? Sub.isValidDrawMove card5 game7 drawMove7,
        testCase "checks if played card is drawn card (2)" $ True @=? Sub.isValidDrawMove card7 game7 drawMove7,
        testCase "handles drawn wild cards correctly" $ True @=? Sub.isValidDrawMove card8 game8 drawMove8]
    ]

{-------------------- main --------------------}
tests :: TestTree
tests = testGroup "Tests" [qcProperties, unitTests, strategyTests]

main :: IO ()
main = defaultMain tests

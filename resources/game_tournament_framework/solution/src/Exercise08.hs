module Exercise08 where

import Data.Bits
import Data.Function
import Data.List
import System.Random (mkStdGen, randoms, randomIO, Random)

-- Player is either 1 or -1
type Player = Int

-- A field is just an Int value where the absolute gives the number of pieces on the field
-- and the sign corresponds to the player
-- e.g. -3 would mean there are three blobs in this field of player -1
type Field = Int

type Row = [Field]
type Column = [Field]
-- boards are rectangles represented as a list of rows
type Board = [Row]

-- A position on the board is represented as (row, column)
-- (0,0) is the top left corner, coordinate values increase towards the bottom right
type Pos = (Int, Int)

-- A size represented as (height,width)
type Size = (Int, Int)

-- A strategy takes the player who's move it is, optionally takes a list of double values
-- to allow for probabilistic strategies, takes the current board and gives back the position
-- of the move the player should do
type Strategy = [Double] -> Player -> Board -> Pos
-- A stateful strategy can additionally pass some object between invocations
type StatefulStrategyFunc a = a -> [Double] -> Player -> Board -> (Pos, a)
-- first value is the state object to pass to the first invocation of each game
type StatefulStrategy a = (a, StatefulStrategyFunc a)

defaultSize :: (Int, Int)
defaultSize = (9,6)

-- Some useful helper functions
row :: Board -> Int -> Row
row = (!!)

column :: Board -> Int -> Column
column = row . transpose

width :: Board -> Int
width (x : _) = length x
width _       = 0

height :: Board -> Int
height = length

size :: Board -> Size
size b = (height b, width b)

getCell :: Pos -> Board -> Field
getCell (y, x) b = b !! y !! x

-- pretty print a single cell
showCell :: Field -> String
showCell c = "- +" !! succ (signum c) : show (abs c)

-- pretty print the given board
showBoard :: Board -> String
showBoard = unlines . map (unwords . map showCell)

-- print a board to the console
printBoard :: Board -> IO ()
printBoard = putStr . showBoard

-- check if a position is one a board of the given size
isValidPos :: Size -> Pos -> Bool
isValidPos (r, c) (y, x) = y >= 0 && y < r && x >= 0 && x < c

{- x.1 -}

-- Check if the given player can put an orb on the given position
canPlaceOrb :: Player -> Pos -> Board -> Bool
canPlaceOrb p (y, x) b = signum (b !! y !! x) /= (-p)

-- Check if the given player has won the game,
-- you can assume that the opponent has made at least one move before
hasWon :: Player -> Board -> Bool
hasWon p = all ((/= (-p)) . signum) . concat

-- the list of neighbors of a cell
neighbors :: Size -> Pos -> [Pos]
neighbors size (y, x) = filter (isValidPos size) [(y, x + 1), (y + 1, x), (y, x - 1), (y - 1, x)]

-- update a single position on the board
-- f: function that modifies the number of orbs in the cell
-- p: player to whom the updated cell should belong
updatePos :: (Int -> Int) -> Player -> Pos -> Board -> Board
updatePos f p (y, x) b = above ++ [left ++ [p * f (abs e)] ++ right] ++ below
  where
    (above, row : below) = splitAt y b
    (left, e : right) = splitAt x row

{- x.2 -}

-- place an orb for the given player in the given cell
putOrb :: Player -> Pos -> Board -> Board
putOrb p pos b
  -- if we have won we don't need to continue updating
  -- hasWon doesn't work on the first move, so catch that
  -- and also return an 0x0 board to skip all remaining updates
  -- note that it is not necessary to signal that the game has ended
  -- and it is not even required to check, using a counter to limit
  -- the number of updates is also possible
  | null b                             = b
  | movesHappened && hasWon p b_placed = []
  -- there is more space in the the target cell, no
  | cell + 1 < length nb               = b_placed
  | otherwise                          = b_reduced
  where
    movesHappened = any (/= 0) $ concat b
    -- board with the requested orb placed
    b_placed = updatePos succ p pos b
    s = size b
    -- the player the cell belonged to before doesn't matter
    -- either it was a valid move or we want to convert it anyways
    cell = abs $ getCell pos b
    nb = neighbors s pos
    -- board with the requested cell cleaned out after an explosion
    -- the exploded orbs have not yet been put back onto the board
    -- note that depending on how exactly you have written this function
    -- it may be possible for more than (length neighbors) orbs in a cell
    b_exploded = updatePos (const 0) p pos b
    -- the board after all explosions have happened or the game has ended
    b_reduced = foldr (putOrb p) b_exploded nb

{- x.3 -}

{-WETT-}

-- creates a list of all fields in a board with their positions
fields :: Board -> [(Pos, Field)]
fields = concatMap (\(r, fs) -> map (\(c, v) -> ((r, c), v)) fs) . zip [0 :: Int ..] . map (zip [0 :: Int ..])

-- pick a random element from a list using Reservoir sampling
pickRandom :: [Double] -> [a] -> a
pickRandom = go (error "list contains no elements") 1
  where
    go _ _ [] _ = error "ran out of random numbers?"
    go e _ _ [] = e
    go e n (r : rs) (x : xs) = go (if r >= (1.0 / n) then e else x) (succ n) rs xs

-- a generic strategy that assigns each possible move a score and then randomly picks one of the best moves
scoringStrategy :: Ord a => ((Pos, Field) -> a) -> Strategy
scoringStrategy score rs p = fst . pickRandom rs . maximumBy (compare `on` (score . head)) . groupBy ((==) `on` score) . filter ((/=) (- p) . signum . snd) . fields

-- this strategy just tries to cause as many reactions as possible (and fails)
strategy :: Strategy
strategy = scoringStrategy (abs . snd)
-- TODO: quality of the solution strategy

data Musterlösung = Musterlösung deriving (Show)
strategyState :: StatefulStrategy Musterlösung
strategyState = (Musterlösung, \s r p b -> (strategy r p b, s))
{-TTEW-}

-- Simulate a game between two strategies on a board of the given size and
-- returns the state of the board before each move together with the player that won the game
play :: [Int] -> Size -> StatefulStrategy a -> StatefulStrategy b -> [(Board, Pos)]
play rss (r, c) (isa, sa) (isb, sb) = go rss isa sa isb sb 1 0 (replicate r (replicate c 0))
  where
    -- type signature is necessary, inferred type is wrong!
    go :: [Int] -> a -> StatefulStrategyFunc a -> b -> StatefulStrategyFunc b -> Player -> Int -> Board -> [(Board, Pos)]
    go (rs:rss) stc sc stn sn p n b
      | won       = []
      | valid     = (b, m) : go rss stn sn st' sc (-p) (succ n) (putOrb p m b)
      | otherwise = []
      where
        won      = n > 1 && hasWon (-p) b
        (m, st') = sc stc (mkRandoms rs) p b
        valid    = isValidPos (size b) m && canPlaceOrb p m b

-- Play a game and print it to the console
playAndPrint :: Size -> StatefulStrategy a -> StatefulStrategy b -> IO ()
playAndPrint size sa sb = do
  seed <- randomIO
  -- let seed = 42
  let moves = play (mkRandoms seed) size sa sb
  putStr $
    unlines (zipWith showState moves $ cycle ['+', '-']) ++ "\n" ++
    (case length moves `mod` 2 of { 1 -> "Winner: +"; 0 -> "Winner: -" }) ++ "\n" ++
    "View at https://vmnipkow16.in.tum.de/christmas2020/embed.html#i" ++ base64 (1 : t size ++ concatMap (t . snd) moves) ++ "\n"
  where
    showState (b, pos) p = showBoard b ++ p : " places at " ++ show pos ++ "\n"
    t (a, b) = [a, b]

mkRandoms :: Random a => Int -> [a]
mkRandoms = randoms . mkStdGen

base64 :: [Int] -> String
base64 xs = case xs of
  []            -> ""
  [a]           -> f1 a : f2 a 0 : "=="
  [a, b]        -> f1 a : f2 a b : f3 b 0 : "="
  a : b : c : d -> f1 a : f2 a b : f3 b c : f4 c : base64 d
  where
    alphabet = (!!) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    f1 a     = alphabet $ shiftR a 2
    f2 a b   = alphabet $ shiftL (a .&. 3 ) 4 .|. shiftR b 4
    f3 b c   = alphabet $ shiftL (b .&. 15) 2 .|. shiftR c 6
    f4 c     = alphabet $ c .&. 63

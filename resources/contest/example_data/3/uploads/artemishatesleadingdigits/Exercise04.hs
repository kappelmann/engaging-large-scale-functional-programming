module Exercise04 where

toN :: Int -> String -> [Int]
toN _ "" = []
toN last ('R' : xs) = h : toN h xs
  where
    h = last + 1
toN last ('L' : xs) = h : toN h xs
  where
    h = last - 1

-- Hide High Lengh roadNumbers
data House = High Int | Hide Int Int Int
  deriving (Show, Eq)

calcPeak :: [House] -> [House]
calcPeak [] = []
-- hhh
calcPeak (High h1 : High h2 : High h3 : xs) = if h1 < h2 && h1 == h3 then Hide h1 2 1 : calcPeak xs else High h1 : calcPeak (High h2 : High h3 : xs)
-- iii
calcPeak (Hide h1 l1 r1 : Hide h2 l2 r2 : Hide h3 l3 r3 : xs) = if h1 == h3 && h1 < h2 then Hide h1 (2 + l1 + l2 + l3) (r1 + r2 + r3 + 4) : calcPeak xs else Hide h1 l1 r1 : calcPeak (Hide h2 l2 r2 : Hide h3 l3 r3 : xs)
-- hih
calcPeak (High h1 : Hide h l r : High h2 : xs) = if h1 == h2 && h1 < h then Hide h1 (l + 2) (r + 1) : calcPeak xs else High h1 : calcPeak (Hide h l r : High h2 : xs)
-- ihh
calcPeak (Hide h1 l1 r1 : High h2 : High h3 : xs) = if h1 == h3 && h1 < h2 then Hide h1 (l1 + 2) (r1 + 2) : calcPeak xs else Hide h1 l1 r1 : calcPeak (High h2 : High h3 : xs)
-- hhi
calcPeak (High h1 : High h2 : Hide h3 l3 r3 : xs) = if h1 == h3 && h1 < h2 then Hide h1 (l3 + 2) (r3 + 2) : calcPeak xs else High h1 : calcPeak (High h2 : Hide h3 l3 r3 : xs)
-- hii
calcPeak (High h1 : Hide h2 l2 r2 : Hide h3 l3 r3 : xs) = if h1 == h3 && h1 < h2 then Hide h1 (l2 + l3 + 2) (r2 + r3 + 2) : calcPeak xs else High h1 : calcPeak (Hide h2 l2 r2 : Hide h3 l3 r3 : xs)
-- iih
calcPeak (Hide h1 l1 r1 : Hide h2 l2 r2 : High h3 : xs) = if h1 == h3 && h1 < h2 then Hide h1 (l1 + l2 + 2) (r1 + r2 + 2) : calcPeak xs else Hide h1 l1 r1 : calcPeak (Hide h2 l2 r2 : High h3 : xs)
-- ihi
calcPeak (Hide h1 l1 r1 : High h2 : Hide h3 l3 r3 : xs) = if h1 == h3 && h1 < h2 then Hide h1 (l1 + l3 + 2) (r1 + r3 + 4) : calcPeak xs else Hide h1 l1 r1 : calcPeak (High h2 : Hide h3 l3 r3 : xs)
calcPeak (x : xs) = x : calcPeak xs

calc :: [House] -> [House]
calc hs = if hs == again then hs else calc again
  where
    again = calcPeak hs

suM :: (Int, Int) -> [House] -> (Int, Int)
suM w [] = w
suM (l, n) (Hide _ hL _ : xs)
  | l == hL = suM (l, n + 1) xs
  | l < hL = suM (hL, 1) xs
  | otherwise = suM (l, n) xs
suM n (x : xs) = suM n xs

rudolph :: String -> (Int, Int)
rudolph s = suM (0, 1) hsssss
  where
    ns = 0 : toN 0 s
    minN = minimum ns
    highNs = map (\w -> w - minN) ns
    maxRealN = maximum highNs
    highHs = map High highNs
    hsssss = calc highHs

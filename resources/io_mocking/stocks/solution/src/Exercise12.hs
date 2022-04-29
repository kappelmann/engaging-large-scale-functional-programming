module Exercise12 where

import Data.List

split :: Eq a => a -> [a] -> [[a]]
split chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

processLine :: String -> (String, Int, Int)
processLine s = 
  let [name, time, price] = split ',' s
  in (name, read time :: Int, read price :: Int)

readTicker :: IO [(String, Int, Int)]
readTicker = do
  line <- getLine
  if line == "quit" 
     then return [] 
     else do
       rest <- readTicker
       return $ processLine line : rest

main :: IO ()
main = do
  (stock, range) <- (\(stock : rs) -> (stock, concat rs)) . words <$> getLine
  let (low, high) = read range
  prices <- readTicker
  let filtered = filter (\(n, t, p) -> n == stock && t >= low && t <= high) prices
      avg = if null filtered then 0 else sum (map (\(_, _, p) -> p) filtered) `div` length filtered
  print avg

{-# LANGUAGE Trustworthy #-}

module System.Random (
  RandomGen(next, split, genRange), StdGen, mkStdGen, getStdRandom, getStdGen, setStdGen, newStdGen,
  Random (random, randomR, randoms,  randomRs, randomIO, randomRIO)
  ) where

import Prelude hiding (IO)
import Mock.System.IO.Internal
import Data.Char
import Data.Int

class Random a where
  randomR :: RandomGen g => (a,a) -> g -> (a,g)
  random :: RandomGen g => g -> (a, g)
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = x : randomRs ival g' where (x,g') = randomR ival g
  randoms :: RandomGen g => g -> [a]
  randoms g = (\(x,g') -> x : randoms g') (random g)
  randomRIO :: (a,a) -> IO a
  randomRIO range = getStdRandom (randomR range)
  randomIO :: IO a
  randomIO = getStdRandom random

instance Random Int where
  randomR (a,b) g = randomIvalInteger (toInteger a, toInteger b) g
  random g        = randomR (minBound,maxBound) g

instance Random Char where
  randomR (a,b) g = 
      case randomIvalInteger (toInteger (ord a), toInteger (ord b)) g of
        (x,g') -> (chr x, g')
  random g    = randomR (minBound,maxBound) g

instance Random Bool where
  randomR (a,b) g = 
      case randomIvalInteger (bool2Int a, bool2Int b) g of
        (x, g') -> (int2Bool x, g')
       where
         bool2Int :: Bool -> Integer
         bool2Int False = 0
         bool2Int True  = 1

         int2Bool :: Int -> Bool
         int2Bool 0  = False
         int2Bool _  = True

  random g = randomR (minBound,maxBound) g
 
instance Random Integer where
  randomR ival g = randomIvalInteger ival g
  random g   = randomR (toInteger (minBound::Int), toInteger (maxBound::Int)) g

instance Random Double where
  randomR ival g = randomIvalDouble ival id g
  random g       = randomR (0::Double,1) g
  
-- hah, so you thought you were saving cycles by using Float?
instance Random Float where
  random g        = randomIvalDouble (0::Double,1) realToFrac g
  randomR (a,b) g = randomIvalDouble (realToFrac a, realToFrac b) realToFrac g


randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case f n 1 rng of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       k = h - l + 1
       b = 2147483561
       n = iLogBase b k

       f 0 acc g = (acc, g)
       f n' acc g =
          let (x,g') = next g in f (n' - 1) (fromIntegral x + acc * b) g'

randomIvalDouble :: (RandomGen g, Fractional a) => (Double, Double) -> (Double -> a) -> g -> (a, g)
randomIvalDouble (l,h) fromDouble rng 
  | l > h     = randomIvalDouble (h,l) fromDouble rng
  | otherwise = 
       case randomIvalInteger (toInteger (minBound::Int32), toInteger (maxBound::Int32)) rng of
         (x, rng') -> 
           let
            scaled_x = 
             fromDouble ((l+h)/2) + 
             fromDouble ((h-l) / realToFrac int32Count) *
             fromIntegral (x::Int32)
           in
             (scaled_x, rng')

int32Count :: Integer
int32Count = toInteger (maxBound::Int32) - toInteger (minBound::Int32) + 1

iLogBase :: Integer -> Integer -> Integer
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)

stdRange :: (Int,Int)
stdRange = (0, 2147483562)


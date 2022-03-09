{-# LANGUAGE  ViewPatterns #-}

module Exercise03 where

import Data.Array.Unboxed
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.List

primeMayor :: Integer -> Integer
primeMayor vv
  | even vv && vv > 2 = 2
  | vv' `IS.member` candidates = 1
  | otherwise = go 0 lows highs
    where
      vv' = fromIntegral vv
      half = vv' `div` 2
      candidates = IS.fromDistinctAscList $ primesToNA vv'
      --takeWhile (<= vv') primesSA
      (lows, highs) = IS.partition (< half) candidates


      go 1000 _ _ = 3
      go fuel ll@(IS.maxView -> Just (l, ls)) hh@(IS.minView -> Just (h, hs))
        | l + h == vv' = 2
        | l + h  > vv' = go (fuel + 1) ls hh
        | l + h  < vv' = go (fuel + 1) ll hs
      go _ _ _ = 3
      -- candidates = S.fromDistinctAscList $ primes vv

primes :: Int -> IS.IntSet
primes limit = IS.fromDistinctAscList $ go [2..fromIntegral limit]
  where
    baseprimes = [2, 3, 5, 7, 11]
    multiples p = [p*p,p*p+p..limit]
    go [] = []
    go (p:ps) = if p*p > limit then p:ps else p : go (ps \\ multiples p)

-- Taken from https://wiki.haskell.org/Prime_numbers

-- primesToNA
primesToNA n = 2: [i | i <- [3,5..n], ar ! i]
  where
    ar = f 5 $ accumArray (\ a b -> False) True (3,n)
                        [(i,()) | i <- [9,15..n]]
    f p a | q > n = a
          | True  = if null x then a2 else f (head x) a2
      where q = p*p
            a2  :: UArray Int Bool
            a2 = a // [(i,False) | i <- [q, q+2*p..n]]
            x  = [i | i <- [p+2,p+4..n], a2 ! i]


primesSA :: [Int]
primesSA = 2 : oddprimes ()
  where
    oddprimes = (3 :) . sieve 3 [] . oddprimes
    sieve x fs (p:ps) = [i*2 + x | (i,True) <- assocs a]
                        ++ sieve (p*p) ((p,0) :
                             [(s, rem (y-q) s) | (s,y) <- fs]) ps
      where
      q = (p*p-x)`div`2
      a :: UArray Int Bool
      a = accumArray (\ b c -> False) True (1,q-1)
                     [(i,()) | (s,y) <- fs, i <- [y+s, y+s+s..q]]
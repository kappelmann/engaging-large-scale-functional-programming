module Exercise01 where

{-H2-}
digitToEo :: Integer -> String
digitToEo 0 = "nul"
digitToEo 1 = "unu"
digitToEo 2 = "du"
digitToEo 3 = "tri"
digitToEo 4 = "kvar"
digitToEo 5 = "kvin"
digitToEo 6 = "ses"
digitToEo 7 = "sep"
digitToEo 8 = "ok"
digitToEo 9 = "nau"

-- This is the basic version, which is sufficient for the homework
numberToEoBasic :: Integer -> String
numberToEoBasic n
  | n < 10    = digitToEo n
  | n < 100   = aux 10   "dek"
  | n < 1000  = aux 100  "cent"
  | otherwise = aux 1000 (if n >= 2000 then " mil" else "mil") -- it's "du mil", not "dumil"!
  where aux base baseString =
          let (q, r) = quotRem n base
              prefix = if q > 1 then numberToEoBasic q else "" -- case distinction to avoid things like "unudek" for "10"
              rest   = if r > 0 then " " ++ numberToEoBasic r else "" -- case distinction do avoid things like "dek nul" for "10"
          in  prefix ++ baseString ++ rest

aux1 :: Integer -> String -> (String, Integer) -> (String, Integer)
aux1 k name (s, n)
  | q == 0    = (s, r)
  | r == 0    = (s ++ prefix, r)
  | otherwise = (s ++ prefix ++ " ", r)
  where base = 1000 ^ k
        (q, r) = quotRem n base
        prefix = numberToEoBasic q ++ " " ++ name ++ if q > 1 then "j" else ""

aux2 :: Integer -> (String, Integer) -> (String, Integer)
aux2 1 x = aux1 2 "miliono" (aux1 3 "miliardo" x)
aux2 k x = aux2 (k - 1) (aux1 (2*k) (name ++ "iliono") (aux1 (2*k+1) (name ++ "iliardo") x))
  where name = numberToEoBasic k

numberToEo :: Integer -> String
numberToEo n = if n < 1000000 then numberToEoBasic n else s ++ if n' == 0 then "" else numberToEoBasic n'
  where (s, n') = aux2 10 ("", n)

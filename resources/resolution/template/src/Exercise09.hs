module Exercise09 where

import Types

import Data.List (sort, sortOn)
import Data.Ord (Down(Down))

-- things from the tutorial

instance Show Polarity where
  show Pos = ""
  show Neg = "~"

instance Show ELiteral where
  show (Literal p a) = show p ++ a

lName :: Literal -> Name
lName (Literal _ n) = n

lPos :: Name -> Literal
lPos = Literal Pos

lNeg :: Name -> Literal
lNeg = Literal Neg

lIsPos :: Literal -> Bool
lIsPos (Literal p _) = p == Pos

lIsNeg :: Literal -> Bool
lIsNeg = not . lIsPos

lNegate :: Literal -> Literal
lNegate (Literal Pos n) = Literal Neg n
lNegate (Literal Neg n) = Literal Pos n

complements :: Literal -> Literal -> Bool
complements (Literal p n) (Literal p' n') = p /= p' && n == n'

evalLiteral :: Valuation -> Literal -> Bool
evalLiteral val l@(Literal _ n)
  | lIsPos l  = n `elem` val
  | otherwise = n `notElem` val

evalClause :: Valuation -> Clause -> Bool
evalClause = any . evalLiteral

eval :: Valuation -> ConjForm -> Bool
eval = all . evalClause

clauseIsTauto :: Clause -> Bool
clauseIsTauto [] = False
clauseIsTauto (l:ls) = lNegate l `elem` ls || clauseIsTauto ls

-- homework starts here

-- feel free to change behaviour and type
resolve :: Name -> Clause -> Clause -> Clause
resolve = undefined

-- feel free to change behaviour and type
resolvants :: KeyClause -> KeyClause -> [(Resolve, Clause)]
resolvants = undefined

-- proof checking

-- don't change the type nor expected behaviour
proofCheck :: EConjForm -> Proof -> Bool
proofCheck = undefined

-- feel free to change behaviour and type
selClause :: SelClauseStrategy 
selClause = undefined

-- feel free to change behaviour and type
resolutionParam :: SelClauseStrategy -> EConjForm -> (Proof, KeyConjForm)
resolutionParam = undefined

{-WETT-}

-- don't change the type; instantiate your best resolution prover here
-- Please leave some comments for the MC Jr to understand your ideas and code :')
resolution :: EConjForm -> (Proof, KeyConjForm)
resolution = resolutionParam selClause

{-TTEW-}

-- extract a model as described on https://lara.epfl.ch/w/_media/sav08/gbtalk.pdf

instance Ord Polarity where
  Neg <= Pos = False
  _ <= _ = True

instance Ord ELiteral where
  (Literal p a) <= (Literal p' a') = a < a' || a == a' && p <= p'

extractModel :: ConjForm -> Valuation
extractModel = run [] . sort . map (sortOn Down)
  where
    run val [] = val
    run val (c:cs)
      | evalClause val c = run val cs
      | otherwise = run (lName (head c):val) cs

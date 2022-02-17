module Exercise09 where

import Types

import Data.Maybe (listToMaybe)
import Control.Arrow (Arrow(second))
import Data.List (nub, sort, sortOn, delete)
import Data.Ord (Down(Down))

-- tutorial starts here
-- util functions and instances

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

-- basic resolution functions

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

-- basic resolution functions

resolve :: Name -> Clause -> Clause -> Clause
resolve n cp cn = filter (/=lPos n) cp ++ filter (/=lNeg n) cn

resolvants :: KeyClause -> KeyClause -> [(Resolve, Clause)]
resolvants kc1@(_,c1) kc2@(_,c2) = 
  [(Resolve n kp kn, resolve n cp cn) | l1<-c1, l2<-c2, l1 `complements` l2,
    let n = lName l1,
    let ((kp,cp), (kn,cn)) = if lIsPos l1 then (kc1, kc2) else (kc2, kc1)]

-- proof checking

proofCheck :: EConjForm -> Proof -> Bool
proofCheck cs (Model val) = eval val cs
proofCheck cs (Refutation []) = [] `elem` cs
proofCheck cs (Refutation (Resolve n k1 k2:rs)) = proofCheck (cs++[c]) (Refutation rs)
  where c = resolve n (cs !! k1) (cs !! k2)

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

selClause :: SelClauseStrategy 
selClause = listToMaybe

-- simplify clauses into canonical form
processClause :: Clause -> Clause
processClause = sort . nub -- nub to remove duplicates, sort to speed-up subsumption checks

subsumes :: Clause -> Clause -> Bool
subsumes c1 c2 = length c1 <= length c2 && all (`elem` c2) c1

notSubsumes :: Clause -> Clause -> Bool
notSubsumes = (not.) . subsumes

preProcess :: ConjForm -> KeyConjForm
preProcess cs = 
  let kcs = zip [(0 :: Key)..] cs -- add the keys
      fkcs = filter (not . clauseIsTauto . snd) kcs in-- filter tautolgical clauses
  map (second processClause) fkcs -- preprocess clauses

resolutionParam :: SelClauseStrategy -> ConjForm -> (Proof, KeyConjForm)
resolutionParam selClause csInit = run [] (length csInit) (preProcess csInit) []
  where
    run :: [Resolve] -> Key -> KeyConjForm -> KeyConjForm -> (Proof, KeyConjForm)
    run rs k ukcs pkcs = case selClause ukcs of
      -- everything is saturated -> extract the model
      Nothing -> (Model $ extractModel $ map snd pkcs, pkcs)
      -- found the empty clause -> return the resolution proof
      Just (_, []) -> (Refutation rs, pkcs ++ ukcs)
      Just ukc@(_,uc) -> 
        let ukcs' = delete ukc ukcs -- remove the clause from the unprocessed set U
            pcs = map snd pkcs in
        if any (`subsumes` uc) pcs then run rs k ukcs' pkcs -- clause subsumed -> continue
        else 
          let pkcs' = filterSubsumed uc pkcs -- remove clauses subsumed by uc
              (nrs, ncs) = unzip [(r, sc) | (r, c) <- concatMap (resolvants ukc) pkcs',
                not $ clauseIsTauto c, -- skip trivial clauses
                let sc = processClause c] -- process clauses
              nukcs = zip [k..] ncs ++ ukcs' in
          run (rs ++ nrs) (k + length ncs) nukcs (ukc:pkcs')
    filterSubsumed :: Clause -> KeyConjForm -> KeyConjForm
    filterSubsumed c = filter (notSubsumes c . snd)

resolution :: EConjForm -> (Proof, KeyConjForm)
resolution = resolutionParam selClause

module Exercise09 where

import Data.List
import Data.Maybe

{-T9.3.1-}
type Name = String
data Atom = T | Var Name
  deriving (Eq, Show)
data Literal = Pos Atom | Neg Atom
  deriving (Eq, Show)
data Form = L Literal | Form :&: Form | Form :|: Form
  deriving (Eq, Show)


{-T9.3.2-}
top :: Literal
top = Pos T

bottom :: Literal
bottom = Neg T

{-T9.3.3-}
type Clause = [Literal]
type ConjForm = [Clause]
clauseToForm :: Clause -> Form
clauseToForm [] = L bottom
clauseToForm ls = foldr ((:|:) . L) (L $ last ls) (init ls)

conjToForm :: ConjForm -> Form
conjToForm [] = L top
conjToForm ds = foldr ((:&:) . clauseToForm) (clauseToForm $ last ds) (init ds)

{-T9.3.4-}
type Valuation = [(Name,Bool)]

substLiteral :: Valuation -> Literal -> Literal
substLiteral v l@(Pos (Var n)) = case lookup n v of
  Just b -> if b then top else bottom
  Nothing -> l
substLiteral v l@(Neg (Var n)) = case lookup n v of
  Just b -> if b then bottom else top
  Nothing -> l
substLiteral v l = l

substClause :: Valuation -> Clause -> Clause
substClause = map . substLiteral

substConj :: Valuation -> ConjForm -> ConjForm
substConj = map . substClause

{-H9.2.1-}
simpClause :: Clause -> Clause
simpClause = foldr collect []
  where
    collect _ [l] | l == top    = [top]
    collect l _   | l == top    = [top]
    collect l acc | l == bottom = acc
    collect a acc               = a:acc

simpConj :: ConjForm -> ConjForm
simpConj = foldr collect [] . map simpClause
  where
    collect _   [[]]           = [[]]
    collect []  _              = [[]]
    collect [l] acc | l == top = acc
    collect a   acc            = a:acc

{-H9.2.2-}
cnf :: Form -> ConjForm
cnf (L l) = [[l]]
cnf (f1 :&: f2) = cnf f1 ++ cnf f2
cnf (f1 :|: f2) = [c1 ++ c2 | c1<-cnf f1, c2<-cnf f2]

{-H9.2.3-}
-- primitive selection strategy: selects the first Variable from the left
-- sets the found variable to True if it occurs positively and False otherwise
selectV :: ConjForm -> Maybe (Name, Bool)
selectV = foldr selectFirst Nothing
  where
    selectFirst ls Nothing = foldr chooseOrSkip Nothing ls
    selectFirst _  res     = res
    chooseOrSkip _ (Just res)    = Just res
    chooseOrSkip (Pos (Var n)) _ = Just (n, True)
    chooseOrSkip (Neg (Var n)) _ = Just (n, False)
    chooseOrSkip _ _ = Nothing

{-H9.2.4-}
satWithSelection :: (ConjForm -> Maybe (Name, Bool)) -> ConjForm -> Maybe Valuation
satWithSelection s f = run [] (simpConj f) 
  where
    run v []   = Just v -- satisfied
    run v [[]] = Nothing -- unsatisfiable
    run v f    = case run newV newF of
      Nothing -> run newV' newF'
      res -> res
      where
        (n,b) = fromJust (s f) -- select assignment for next variable
        -- new valuation and formula with selected assignment of next variable
        newF  = simpConj $ substConj [(n,b)] f
        newV  = (n,b):v
        -- new valuation and formula with the negation of selected assignment of next variable
        newF' = simpConj $ substConj [(n,not b)] f
        newV' = (n,not b):v

satConj :: ConjForm -> Maybe Valuation
satConj = satWithSelection selectV

{-H9.2.5-}
sat :: Form -> Maybe Valuation
sat = satConj . cnf

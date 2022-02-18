module Exercise09 where

type Name = String
type Valuation = [(Name,Bool)]
data Atom = T | Var Name
  deriving (Eq, Show)
data Literal = Pos Atom | Neg Atom
  deriving (Eq, Show)
data Form = L Literal | Form :&: Form | Form :|: Form
  deriving (Eq, Show)
type Clause = [Literal]
type ConjForm = [Clause]

{-T9.3.2-}
top :: Literal
top = Pos T

bottom :: Literal
bottom = Neg T

{-T9.3.3-}
clauseToForm :: Clause -> Form
clauseToForm [] = L bottom
clauseToForm ls = foldr ((:|:) . L) (L $ last ls) (init ls)

conjToForm :: ConjForm -> Form
conjToForm [] = L top
conjToForm ds = foldr ((:&:) . clauseToForm) (clauseToForm $ last ds) (init ds)

{-T9.3.4-}
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
simpConj :: ConjForm -> ConjForm
simpConj = id

{-H9.2.2-}
cnf :: Form -> ConjForm
cnf = undefined

{-H9.2.3-}
selectV :: ConjForm -> Maybe (Name, Bool)
selectV = undefined

{-H9.2.5-}
{-WETT-}
satConj :: ConjForm -> Maybe Valuation
satConj = undefined
{-TTEW-}

{-H9.2.6-}
sat :: Form -> Maybe Valuation
sat = undefined


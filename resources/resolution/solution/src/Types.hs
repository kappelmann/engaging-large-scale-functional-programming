module Types where

-- type definitions

type EName = String
type Name = EName -- change as you like
data Polarity = Pos | Neg
  deriving Eq
data ELiteral = Literal Polarity EName
  deriving Eq
type Literal = ELiteral -- change as you like
type EClause = [ELiteral]
type Clause = EClause -- change as you like
type EConjForm = [EClause]
type ConjForm = EConjForm -- change as you like

type Key = Int
type EKeyClause = (Key, EClause)
type KeyClause = EKeyClause -- change as you like
type EKeyConjForm = [EKeyClause]
type KeyConjForm = EKeyConjForm -- change as you like

data EResolve = Resolve EName Key Key
  deriving (Eq, Show)
type Resolve = EResolve -- change as you like
type EValuation = [EName]
type Valuation = EValuation -- change as you like
data EProof = Refutation [EResolve] | Model EValuation
  deriving (Eq, Show)
type Proof = EProof -- change as you like

type SelClauseStrategy = KeyConjForm -> Maybe KeyClause  -- change as you like

-- adapt the following if you changed the internal data types above

toEName :: Name -> EName
toEName = id

toName :: EName -> Name
toName = id

toELiteral :: Literal -> ELiteral
toELiteral = id

toLiteral :: ELiteral -> Literal
toLiteral = id

toEClause :: Clause -> EClause
toEClause = id

toClause :: EClause -> Clause
toClause = id

toEConjForm :: ConjForm -> EConjForm
toEConjForm = id

toConjForm :: EConjForm -> ConjForm
toConjForm = id

toEKeyClause :: KeyClause -> EKeyClause
toEKeyClause = id

toKeyClause :: EKeyClause -> KeyClause
toKeyClause = id

toEKeyConjForm :: KeyConjForm -> EKeyConjForm
toEKeyConjForm = id

toKeyConjForm :: EKeyConjForm -> KeyConjForm
toKeyConjForm = id

toEResolve :: Resolve -> EResolve
toEResolve = id

toResolve :: EResolve -> Resolve
toResolve = id

toEValuation :: Valuation -> EValuation
toEValuation = id

toValuation :: EValuation -> Valuation
toValuation = id

toEProof :: Proof -> EProof
toEProof = id

toProof :: EProof -> Proof
toProof = id


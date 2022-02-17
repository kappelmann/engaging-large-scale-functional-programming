module Types_Efficient where

import Data.Maybe (fromJust)
import qualified Data.IntSet as IS
import qualified Data.IntMap.Lazy as IM
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Function (on)
import Control.Arrow (Arrow(second))

-- type definitions

type EName = String
type Name = Int -- change as you like
data Polarity = Pos | Neg
  deriving Eq
data ELiteral = Literal Polarity EName
  deriving Eq
type Literal = Name -- change as you like
type EClause = [ELiteral]
type Clause = IS.IntSet -- change as you like
type EConjForm = [EClause]
type ConjForm = IM.IntMap Clause -- change as you like

type Key = Int
type EKeyClause = (Key, EClause)
type KeyClause = (Key, Clause) -- change as you like
type Data = (Name, Name) -- the smallest value and the largest literal
type DataClause = (Data, Clause)
type KeyDataClause = (Key, DataClause)
type EKeyConjForm = [EKeyClause]
type KeyConjForm = IM.IntMap Clause -- change as you like
type KeyDataConjForm = IM.IntMap DataClause -- change as you like

data EResolve = Resolve EName Key Key
  deriving (Eq, Show)
data Resolve = Resolve' Name Key Key -- change as you like
  deriving (Eq, Show)
type EValuation = [EName]
type Valuation = IS.IntSet -- change as you like
data EProof = Refutation [EResolve] | Model EValuation
  deriving (Eq, Show)
data Proof = Refutation' [Resolve] | Model' Valuation -- change as you like
  deriving (Eq, Show)

-- queue where key = size of clauses
type Queue = IM.IntMap KeyDataConjForm
type SelClauseStrategy = Queue -> Maybe (KeyDataClause, Queue)  -- change as you like

-- selection strategies for negative clauses
type SelLiteralsStrategy = DataClause -> Clause

-- adapt the following if you changed the internal data types above

toEName :: Name -> EName
toEName = show

toName :: EName -> Name
toName = fst . fromJust . LC.readInt . LC.pack -- readInt is way faster than Prelude.read

toELiteral :: Literal -> ELiteral
toELiteral n
  | n < 0 = Literal Neg $ toEName (abs n)
  | otherwise = Literal Pos $ toEName n

toLiteral :: ELiteral -> Literal
toLiteral (Literal Pos n) = toName n
toLiteral (Literal Neg n) = -(toName n)

toEClause :: Clause -> EClause
toEClause = map toELiteral . IS.elems

toClause :: EClause -> Clause
toClause = IS.fromList . map toLiteral

toEConjForm :: ConjForm -> EConjForm
toEConjForm = map (toEClause . snd) . IM.toList

toConjForm :: EConjForm -> ConjForm
toConjForm = IM.fromAscList . zipWith (curry $ second toClause) [0..]

toEKeyClause :: KeyClause -> EKeyClause
toEKeyClause = second toEClause 

toKeyClause :: EKeyClause -> KeyClause
toKeyClause = second toClause

toEKeyConjForm :: KeyConjForm -> EKeyConjForm
toEKeyConjForm = map toEKeyClause . IM.toList

toKeyConjForm :: EKeyConjForm -> KeyConjForm
toKeyConjForm = IM.fromList . map toKeyClause

toEResolve :: Resolve -> EResolve
toEResolve (Resolve' n k1 k2) = Resolve (toEName n) k1 k2

toResolve :: EResolve -> Resolve
toResolve (Resolve n k1 k2)= Resolve' (toName n) k1 k2

toEValuation :: Valuation -> EValuation
toEValuation = map toEName . IS.toList

toValuation :: EValuation -> Valuation
toValuation = IS.fromList . map toName

toEProof :: Proof -> EProof
toEProof (Refutation' rs) = Refutation $ map toEResolve rs
toEProof (Model' val) = Model $ toEValuation val

toProof :: EProof -> Proof
toProof (Refutation rs) = Refutation' $ map toResolve rs
toProof (Model val) = Model' $ toValuation val

queueToKeyConjForm :: Queue -> KeyConjForm
queueToKeyConjForm = IM.foldr (IM.union . IM.map snd) IM.empty 

lData :: Clause -> Data
lData c 
  | IS.null c = (0,0) -- don't care value
  | otherwise =
    let lma = IS.findMax c
        lmi = IS.findMin c in
     if ((>) `on` abs) lma lmi then (lmi,lma) else (lmi,lmi)

keyConjFormToQueue :: KeyConjForm -> Queue
keyConjFormToQueue = IM.foldrWithKey (\k c -> IM.insertWith IM.union (IS.size c) (IM.singleton k (lData c,c))) IM.empty

module Interface (
  module Interface,
  T.EName,
  T.Polarity (..),
  T.ELiteral (..),
  T.EClause,
  T.EConjForm,
  T.Key,
  T.EKeyClause,
  T.EKeyConjForm,
  T.EResolve (..),
  T.EValuation,
  T.EProof (..)
) where

import qualified Exercise09 as E
import qualified Types as T

toEName :: T.Name -> T.EName
toEName = T.toEName

toName :: T.EName -> T.Name
toName = T.toName

toELiteral :: T.Literal -> T.ELiteral
toELiteral = T.toELiteral

toLiteral :: T.ELiteral -> T.Literal
toLiteral = T.toLiteral

toEClause :: T.Clause -> T.EClause
toEClause = T.toEClause

toClause :: T.EClause -> T.Clause
toClause = T.toClause

toEConjForm :: T.ConjForm -> T.EConjForm
toEConjForm = T.toEConjForm

toConjForm :: T.EConjForm -> T.ConjForm
toConjForm = T.toConjForm

toEKeyClause :: T.KeyClause -> T.EKeyClause
toEKeyClause = T.toEKeyClause

toKeyClause :: T.EKeyClause -> T.KeyClause
toKeyClause = T.toKeyClause

toEKeyConjForm :: T.KeyConjForm -> T.EKeyConjForm
toEKeyConjForm = T.toEKeyConjForm

toKeyConjForm :: T.EKeyConjForm -> T.KeyConjForm
toKeyConjForm = T.toKeyConjForm

toEResolve :: T.Resolve -> T.EResolve
toEResolve = T.toEResolve

toResolve :: T.EResolve -> T.Resolve
toResolve = T.toResolve

toEValuation :: T.Valuation -> T.EValuation
toEValuation = T.toEValuation

toValuation :: T.EValuation -> T.Valuation
toValuation = T.toValuation

toEProof :: T.Proof -> T.EProof
toEProof = T.toEProof

toProof :: T.EProof -> T.Proof
toProof = T.toProof

proofCheck :: T.EConjForm -> T.Proof -> Bool
proofCheck = E.proofCheck

resolution :: T.EConjForm -> (T.Proof, T.KeyConjForm)
resolution = E.resolution

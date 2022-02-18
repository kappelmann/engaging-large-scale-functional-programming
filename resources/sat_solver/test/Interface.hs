module Interface (
  module Interface,
  Exercise09.Name,
  Exercise09.Atom (..),
  Exercise09.Literal (..),
  Exercise09.Form (..),
  Exercise09.Clause,
  Exercise09.ConjForm,
  Exercise09.Valuation
) where

import qualified Exercise09

{-H9.2.1-}
simpConj :: Exercise09.ConjForm -> Exercise09.ConjForm
simpConj = Exercise09.simpConj

{-H9.2.2-}
cnf :: Exercise09.Form -> Exercise09.ConjForm
cnf = Exercise09.cnf

{-H9.2.3-}
selectV :: Exercise09.ConjForm -> Maybe (Exercise09.Name, Bool)
selectV = Exercise09.selectV

{-H9.2.4-}
satConj :: Exercise09.ConjForm -> Maybe Exercise09.Valuation
satConj = Exercise09.satConj

{-H9.2.5-}
sat :: Exercise09.Form -> Maybe Exercise09.Valuation
sat = Exercise09.sat

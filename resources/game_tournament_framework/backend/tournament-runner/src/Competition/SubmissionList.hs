{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Competition.SubmissionList (submissions) where

import Competition.Game.StrategyWrapper (wrapStrategyEvaluation)
import Competition.Types (Submission (..))

#include "SubmissionList.Imports.generated"

submissions :: [Submission]
submissions = tail
  [ undefined
#include "SubmissionList.generated"
  ]

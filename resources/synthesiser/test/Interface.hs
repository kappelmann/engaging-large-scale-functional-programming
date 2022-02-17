module Interface where

import qualified Exercise06 as E
import qualified Types as T

sinPeriod :: T.Signal
sinPeriod = E.sinPeriod

sqwPeriod :: T.Signal
sqwPeriod = E.sqwPeriod

sawPeriod :: T.Signal
sawPeriod = E.sawPeriod

triPeriod :: T.Signal
triPeriod = E.triPeriod

silence :: T.Signal
silence = E.silence

osc :: T.Signal -> T.ADSR -> T.Oscillator
osc = E.osc

adsr :: T.ADSR -> T.Seconds -> T.Signal -> T.Signal
adsr = E.adsr

mix :: [T.SampledSignal] -> T.SampledSignal
mix = E.mix

module Exercise06 where

import Data.List

import Effects
import Types

sinPeriod :: Signal
sinPeriod t = sin (2 * pi * t)

sqwPeriod :: Signal
sqwPeriod t
  | t < 0.5 = -1
  | t == 0.5 = 0
  | t > 0.5 = 1

sawPeriod :: Signal
sawPeriod t = 2 * t - 1

triPeriod :: Signal
triPeriod t
  | t < 0.5 = 4 * t - 1
  | otherwise = -4 * t + 3

silence :: Signal
silence = const 0

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitone -> Hz
f n = 440.0 * (2 ** (fromInteger n / 12.0))

osc :: Signal -> ADSR -> Oscillator
osc periodSig adsrParams semitone duration = envelope cycledSig
 where
    envelope = adsr adsrParams duration
    freq = f semitone
    cycledSig t = let t' = t * freq in periodSig (t' - fromIntegral (floor t'))

adsr :: ADSR -> Seconds -> Signal -> Signal
adsr (a,d,s,r) duration signal t =
    envelope t * signal t where
      envelope t
        | t < a = t / a
        | t < a + d = 1 - (1 - s) * (t - a) / d
        | t < duration - r = s
        | otherwise = s * (duration - t) / r

mix :: [SampledSignal] -> SampledSignal
mix signals = map (/n) $ combineSignals signals
  where
    n = fromIntegral $ length signals

    combineSignals :: [SampledSignal] -> SampledSignal
    combineSignals []     = []
    combineSignals (x:xs) = addSignals x (combineSignals xs)

    addSignals :: SampledSignal -> SampledSignal -> SampledSignal
    addSignals [] [] = []
    addSignals [] ys = ys
    addSignals xs [] = xs
    addSignals (x:xs) (y:ys) = (x + y) : addSignals xs ys

piano :: Oscillator
piano = osc sawPeriod (0.01, 0.1, 0.7, 0.2)

lead :: Oscillator
lead = osc sqwPeriod (0.01, 0.2, 0.3, 0.1)

bass :: Oscillator
bass = osc sinPeriod (0.001, 0.2, 0.9, 0.1)

mixTracks :: [SampledSignal] -> [Double] -> SampledSignal
mixTracks trks vols = mix $ zipWith (\trk vol -> map (* vol) trk) trks vols

notesToSignal :: (Oscillator -> [Note] -> SampledSignal) -> [[Note]] -> SampledSignal
notesToSignal playNotes tracks = audio
  where
    instrs = cycle [piano, bass]
    audioTracks = zipWith playNotes instrs tracks
    audioNoEffects = mixTracks audioTracks [0.8, 0.7]
    audio = audioNoEffects
    -- To add effects, replace the line above by the line at the end of this comment and change the list off effects to your own effects.
    -- Note that the effects are applied from right to left, ie. the rightmost effect is applied first.
    -- audio = addEffects [clip 0.9, addGain 4] audioNoEffects

    -- If you want to apply an effect only to parts of the signal, you can use the function applyEffectToInterval, as shown below.
    -- Thereby the first argument specifies the interval to which the effect should be applied in seconds.
    -- audio = applyEffectToInterval (2, 6) audioNoEffects distortion


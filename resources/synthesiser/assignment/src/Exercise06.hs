module Exercise06 where

import Data.List

import Effects
import Types

sinPeriod :: Signal
sinPeriod = undefined

sqwPeriod :: Signal
sqwPeriod = undefined

sawPeriod :: Signal
sawPeriod = undefined

triPeriod :: Signal
triPeriod = undefined

silence :: Signal
silence = undefined

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitone -> Hz
f n = 440.0 * (2 ** (fromInteger n / 12.0))

osc :: Signal -> ADSR -> Oscillator
osc = undefined

adsr :: ADSR -> Seconds -> Signal -> Signal
adsr = undefined

mix :: [SampledSignal] -> SampledSignal
mix = undefined

{-WETT-}

-- you can add new oscillators here
piano :: Oscillator
piano = osc sawPeriod (0.01, 0.1, 0.7, 0.2)

lead :: Oscillator
lead = osc sqwPeriod (0.01, 0.2, 0.3, 0.1)

bass :: Oscillator
bass = osc sinPeriod (0.001, 0.2, 0.9, 0.1)

-- you can add more effects here



-- mix the signals with some volume
mixTracks :: [SampledSignal] -> [Double] -> SampledSignal
mixTracks trks vols = mix $ zipWith (\trk vol -> map (* vol) trk) trks vols

-- here we mix it all together and apply the effects
notesToSignal :: (Oscillator -> [Note] -> SampledSignal) -> [[Note]] -> SampledSignal
notesToSignal playNotes tracks = audio
  where
    -- specify the instruments of each track
    -- try the following instruments with the mario.mid file
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
{-TTEW-}

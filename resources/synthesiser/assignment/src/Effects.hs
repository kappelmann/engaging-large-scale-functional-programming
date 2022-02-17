module Effects where
    
import Data.List
import Types

addEffects :: [DSPEffect] -> SampledSignal -> SampledSignal
addEffects = foldr (.) id

applyEffectToInterval :: (Seconds, Seconds) -> SampledSignal -> DSPEffect -> SampledSignal
applyEffectToInterval (from, to) signal effect
  -- assert that from >= 0, to - from >= 0, to <= length signal and simply return original signal if constraints aren't met
  | from < 0 || from > to || samplesPerSecond to > length signal = signal
  | otherwise =
    samplesBefore ++ samplesDuring ++ samplesAfter where
      samplesBefore = take nSamplesBefore signal -- unaffected
      samplesDuring = effect $ take nSamplesDuring $ drop nSamplesBefore signal -- effected
      samplesAfter  = drop (nSamplesBefore + nSamplesDuring) signal -- unaffected

      nSamplesBefore = samplesPerSecond from
      nSamplesDuring = samplesPerSecond (to - from)

gain :: Double
gain = 2.0

clippingThreshold :: Double
clippingThreshold = 0.8

clip :: Double -> DSPEffect
clip threshold = map (\sample -> if sample > 0 then min threshold sample else max (negate threshold) sample) -- clip positive and negative samples

addGain :: Double -> DSPEffect
addGain value = map (* value)

distortion :: DSPEffect
distortion = clip clippingThreshold . addGain gain

-- add more effects here

module Types where

type Seconds = Double
type Sample = Double
type Signal = Seconds -> Sample
type SampledSignal = [Sample]
type Hz = Double
type Semitone = Integer
type Note = (Semitone, Seconds, Seconds)
type Oscillator = Semitone -> Seconds -> Double -> Double
type ADSR = (Seconds, Seconds, Sample, Seconds) 
type DSPEffect = SampledSignal -> SampledSignal

sampleRate :: Double
sampleRate = 48000

samplesPerPeriod :: Hz -> Int
samplesPerPeriod hz = round $ sampleRate / hz

samplesPerSecond :: Seconds -> Int
samplesPerSecond duration = round $ duration * sampleRate 


module Main where

import Codec.Midi
import Codec.Wav
import Data.Array.Unboxed
import Data.Int
import Data.Audio
import System.Environment
import System.Exit
import Control.Monad
import Control.DeepSeq

import Exercise06
import Synth
import Types

signalToSampleData :: SampledSignal -> SampleData Int16
signalToSampleData signal = 
  listArray (0, n) $ map fromSample signal 
  where
    n = length signal - 1

limit :: SampledSignal -> SampledSignal
limit = map (min 1.0 . max (-1.0))

save :: FilePath -> SampledSignal -> IO ()
save filePath signal = 
  let sampleData = signalToSampleData $ (force . limit) signal
      audio = Audio {Data.Audio.sampleRate = round Types.sampleRate, channelNumber = 1, sampleData = sampleData}
  in
    Codec.Wav.exportFile filePath audio

parseAndSave :: FilePath -> Midi -> IO ()
parseAndSave filepath midi = save filepath audio
    where
        tracks = dropEmptyTracks $ midiToNotes midi
        audio = Exercise06.notesToSignal playNotes tracks

printHelp :: IO ()
printHelp = do
    progname <- getProgName
    putStrLn $ "Usage: " ++ progname ++ " <path to MIDI file> <path to output WAV file>"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) printHelp
    let (midipath:wavpath:_) = args
    midi <- Codec.Midi.importFile midipath
    either putStrLn (parseAndSave wavpath) midi


module Synth where

import Codec.Midi
import Data.List
import Control.DeepSeq
import Types

playNotes :: Oscillator -> [Note] -> SampledSignal
playNotes _ [] = []
playNotes osc notes = reverse $ map (/ fromIntegral n) sampledSig
  where
    (n, _, _, sampledSig) = foldl' h (0, [], notes, []) [0..samplesPerSecond $ endTime $ last notes]
    endTime (_, start, duration) = start + duration
    startTime (_, start, _) = start
    osc' (tone, _, duration) = osc tone duration
    h (n, [], [], xs) i = (n, [], [], 0:xs)
    h (n, sigs, notes, xs) i = let
        t = fromIntegral i / sampleRate
        (nextNotes, notes') = break (\note -> startTime note > t) notes
        sigs' = map (\note -> (osc' note . (\t -> t - startTime note), endTime note)) nextNotes ++ filter (\(_, end) -> t <= end) sigs
        n' = max n $ length sigs'
        sample = sum (map (($ t) . fst) sigs')
      in
        (n', sigs', notes', sample:xs)

midiNoteToSemitone :: Key -> Semitone
midiNoteToSemitone k = fromIntegral $ k - 69

timeTrackToNotes :: Track Time -> [Note]
timeTrackToNotes xs = notes
  where
    notes = sortBy (\(_,t1,_) (_,t2,_) -> compare t1 t2) $ concat notesPerNoteF
    notesPerNote = groupBy (\(_,x) (_,y) -> key x == key y) $ sortBy (\(_,x) (_,y) -> compare (key x) (key y)) xs 
    notesPerNoteF = map format notesPerNote
    format [] = []
    format ((ts, n1):(te, n2):xs)
      | isNoteOn n1 && (isNoteOff n2 || velocity n2 == 0) = 
        let k = midiNoteToSemitone $ key n1
        in (k,ts,te-ts) : format xs
      | otherwise = error "Wrong MIDI-Format"

midiToNotes :: Midi -> [[Note]]
midiToNotes midi = force $ map timeTrackToNotes timeTracks
    where
        timeTracks = 
            map 
            (filter (\ (_, x) -> isNoteOn x || isNoteOff x) . toAbsTime . toRealTime  (timeDiv midi))
            (tracks midi)

dropEmptyTracks :: [[Note]] -> [[Note]]
dropEmptyTracks = filter (not . null)

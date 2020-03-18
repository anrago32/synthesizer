-- Audio
-- Description goes here
-- Written by Alex Rago, 2020

module Audio where

import Sound.Pulse.Simple
import Pitch

createPlayer :: IO Simple
createPlayer = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  player <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  return player

playChord :: [Float] -> Float
playChord tones = sum tones

-- cMajor :: Int -> Float
-- cMajor time = generateChord [tone1, tone2, tone3]
--   where tone1 = sineOscillator (C, 5) time
--         tone2 = sineOscillator (E, 5) time
--         tone3 = sineOscillator (G, 5) time
--
-- fMajor :: Int -> Float
-- fMajor time = generateChord [tone1, tone2, tone3]
--   where tone1 = sineOscillator (F, 5) time
--         tone2 = sineOscillator (A, 5) time
--         tone3 = sineOscillator (C, 6) time

-- Envelope Specification
-- Key Pressed       -> A(t) = t / a
-- Volume == 1       -> D(t) = 1 + (a - t) / d
-- Volume == s / 100 -> S(t) = s / 100
-- Key Released      -> R(t) = endVolume / 100 + (endTime - t) / r

sineOsc :: Pitch -> Int -> Float
sineOsc pitch time = sin phase * 0.2
  where phase = fromIntegral time * frequency * pi * 2.0 / sampleRate
        frequency = standardPitch pitch
        sampleRate = 48000.0

-- fmOsc :: Pitch -> Int -> Float
-- fmOsc pitch time = sin (sineOsc pitch time * 2) * 0.25

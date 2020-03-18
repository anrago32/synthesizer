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

-- Envelope Specification
-- E(t) = 0 < t < a     -> A(t) = t / a
--        a < t < a + d -> D(t) = 1 + (100 - s) * (a - t) / (100 * d)
--        a + d < t < m -> S(t) = s / 100
--        m < t < m + r -> R(t) = v / 100 + v * (m - t) / (100 * r)

-- a: Attack Intensity
-- d: Decay Intensity
-- s: Sustain Volume
-- r: Release Intensity

-- m: Release Time
-- v: Release Volume

sineOsc :: Pitch -> Int -> Float
sineOsc pitch time = sin phase * 0.2
  where phase = fromIntegral time * frequency * pi * 2.0 / sampleRate
        frequency = standardPitch pitch
        sampleRate = 48000.0

-- FM Oscillator Prototype
-- fmOsc :: Pitch -> Int -> Float
-- fmOsc pitch time = sin (sineOsc pitch time * 2) * 0.25

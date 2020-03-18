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
-- A(t) = t / a
-- D(t) = 1 + (a - t) / d
-- S(t) = s / 100
-- R(t) = endVolume / 100 + (endTime - t) / r

-- E(t) = 0 < t < a                   -> A(t)
--        a < t < a + d - d * s / 100 -> D(t)
--        a + d - d * s / 100 < t < m -> S(t)
--        m < t < m + r * v / 100     -> R(t)

-- a: Attack Slope
-- d: Decay Slope
-- s: Sustain Level
-- r: Release Slope

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

-- Audio
-- Description goes here
-- Written by Alex Rago, 2020

module Audio where


import Pitch


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
--
-- generateChord :: [Float] -> Float
-- generateChord tones = (sum tones) / (length tones)


sineOsc :: Pitch -> Int -> Float
sineOsc pitch time = sin phase * 0.5
  where phase = fromIntegral time * frequency * pi * 2.0 / sampleRate
        frequency = standardPitch pitch
        sampleRate = 48000.0


fmOsc :: Pitch -> Int -> Float
fmOsc pitch time = sin (sineOsc pitch time * 2) * 0.5

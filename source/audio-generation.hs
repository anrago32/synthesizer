module AudioGeneration where

import PitchConversion

cMajor :: Int -> Float
cMajor time = generateChord [tone1, tone2, tone3]
  where tone1 = generateTone (C, 5) time
        tone2 = generateTone (E, 5) time
        tone3 = generateTone (G, 5) time

fMajor :: Int -> Float
fMajor time = generateChord [tone1, tone2, tone3]
  where tone1 = generateTone (F, 5) time
        tone2 = generateTone (A, 5) time
        tone3 = generateTone (C, 6) time

generateChord :: [Float] -> Float
generateChord [tone] = tone
generateChord (tone:tones) = (tone + generateChord tones) / 2.0

generateTone :: Pitch -> Int -> Float
generateTone pitch time = sin period
  where frequency = standardPitch pitch
        period = fromIntegral time * frequency * pi * 2.0 / sampleRate
        sampleRate = 48000.0

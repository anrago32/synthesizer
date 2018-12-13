module AudioGeneration where

import PitchConversion

cMajor :: Int -> Float
cMajor time = generateChord [tone1, tone2, tone3]
  where tone1 = generateTone (C, 5) time
        tone2 = generateTone (E, 5) time
        tone3 = generateTone (G, 5) time

generateChord :: [Float] -> Float
generateChord [tone] = tone
generateChord (tone:tones) = (tone + generateChord tones) / 2

generateTone :: Pitch -> Int -> Float
generateTone pitch time = sin period
  where frequency = standardPitch pitch
        period = fromIntegral time * frequency * pi * 2 / sampleRate
        sampleRate = 48000

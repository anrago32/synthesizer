module ToneGeneration where

import PitchConversion

generateChord :: [Float] -> Float
generateChord [tone] = tone
generateChord (tone:tones) = (tone + generateChord tones) / 2

generateTone :: Pitch -> Int -> Float
generateTone pitch time = sin period
  where frequency = standardPitch pitch
        period = fromIntegral time * frequency * pi * 2 / sampleRate
        sampleRate = 48000

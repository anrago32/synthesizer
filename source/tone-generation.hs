module ToneGeneration where

import PitchConversion

generateChord :: [[Float]] -> [Float]
generateChord [tone] = tone
generateChord (tone:tones) = map (/2) (zipWith (+) tone (generateChord tones))

generateTone :: Pitch -> [Int] -> [Float]
generateTone pitch times = map wave times
  where frequency = scientificPitch pitch
        period time = fromIntegral time * frequency * pi * 2 / sampleRate
        sampleRate = 48000
        wave time = sin $ period time

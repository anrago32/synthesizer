module AudioGeneration where

import Pitch

generateAudio :: Pitch -> Int -> Double
generateAudio pitch time = sin period * amplitude
  where amplitude = 10
        frequency = scientificPitch pitch
        period = frequency * fromIntegral time * pi * 2 / sampleRate
        sampleRate = 48000.0

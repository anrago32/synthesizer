module Audio where

import Pitch

{-
cMajor :: Int -> Float
cMajor time = generateChord [tone1, tone2, tone3]
  where tone1 = sineOscillator (C, 5) time
        tone2 = sineOscillator (E, 5) time
        tone3 = sineOscillator (G, 5) time

fMajor :: Int -> Float
fMajor time = generateChord [tone1, tone2, tone3]
  where tone1 = sineOscillator (F, 5) time
        tone2 = sineOscillator (A, 5) time
        tone3 = sineOscillator (C, 6) time

generateChord :: [Float] -> Float
generateChord [tone] = tone
generateChord (tone:tones) = (tone + generateChord tones) / 2.0
-}

sineOscillator :: Float -> Int -> Float
sineOscillator frequency time = sin phase
  where phase = fromIntegral time * frequency * pi * 2.0 / sampleRate
        sampleRate = 48000.0

finalOutput :: Int -> Float
finalOutput time = sin (sineOscillator 440.0 time * 2)

-- Pitch
-- Description goes here
-- Written by Alex Rago, 2020

module Pitch where

type Pitch = (Note, Int)
data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

intToPitch :: Int -> Pitch
intToPitch number
  | note == 0  = (C , octave)
  | note == 1  = (Cs, octave)
  | note == 2  = (D , octave)
  | note == 3  = (Ds, octave)
  | note == 4  = (E , octave)
  | note == 5  = (F , octave)
  | note == 6  = (Fs, octave)
  | note == 7  = (G , octave)
  | note == 8  = (Gs, octave)
  | note == 9  = (A , octave)
  | note == 10 = (As, octave)
  | note == 11 = (B , octave)
  where note = number `mod` 12
        octave = number `div` 12

pitchToInt :: Pitch -> Int
pitchToInt (C , octave) = octave * 12 + 0
pitchToInt (Cs, octave) = octave * 12 + 1
pitchToInt (D , octave) = octave * 12 + 2
pitchToInt (Ds, octave) = octave * 12 + 3
pitchToInt (E , octave) = octave * 12 + 4
pitchToInt (F , octave) = octave * 12 + 5
pitchToInt (Fs, octave) = octave * 12 + 6
pitchToInt (G , octave) = octave * 12 + 7
pitchToInt (Gs, octave) = octave * 12 + 8
pitchToInt (A , octave) = octave * 12 + 9
pitchToInt (As, octave) = octave * 12 + 10
pitchToInt (B , octave) = octave * 12 + 11

scientificPitch :: Pitch -> Float
scientificPitch pitch = 256.0 * (2.0 ** ((noteNumber - 48.0) / 12.0))
  where noteNumber = fromIntegral $ pitchToInt pitch :: Float

standardPitch :: Pitch -> Float
standardPitch pitch = 440.0 * (2.0 ** ((pitchNumber - 57.0) / 12.0))
  where pitchNumber = fromIntegral $ pitchToInt pitch :: Float

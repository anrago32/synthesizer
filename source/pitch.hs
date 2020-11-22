module Pitch where

type Pitch = (Name, Int)

data Name = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Eq, Ord, Show)

intToPitch :: Int -> Pitch
intToPitch number
  | name == 0  = (C , octave)
  | name == 1  = (Cs, octave)
  | name == 2  = (D , octave)
  | name == 3  = (Ds, octave)
  | name == 4  = (E , octave)
  | name == 5  = (F , octave)
  | name == 6  = (Fs, octave)
  | name == 7  = (G , octave)
  | name == 8  = (Gs, octave)
  | name == 9  = (A , octave)
  | name == 10 = (As, octave)
  | name == 11 = (B , octave)
  where name = mod number 12
        octave = div number 12

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

module Main where

main :: IO ()
main = do
  print . noteToInt $ (A, 4)
  print . noteFreq  $ (A, 4)
  print . noteFreq  . intToNote $ 57
  print . noteFreq  . intToNote . noteToInt $ (A, 4)

-- Note Definition
type Note = (Pitch, Octave)

data Pitch = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

type Octave = Int

intToNote :: Int -> Note
intToNote number
  | pitch == 0  = (C , octave)
  | pitch == 1  = (Cs, octave)
  | pitch == 2  = (D , octave)
  | pitch == 3  = (Ds, octave)
  | pitch == 4  = (E , octave)
  | pitch == 5  = (F , octave)
  | pitch == 6  = (Fs, octave)
  | pitch == 7  = (G , octave)
  | pitch == 8  = (Gs, octave)
  | pitch == 9  = (A , octave)
  | pitch == 10 = (As, octave)
  | pitch == 11 = (B , octave)
  where pitch = number `mod` 12
        octave = number `div` 12

noteToInt :: Note -> Int
noteToInt (C , octave) = octave * 12 + 0
noteToInt (Cs, octave) = octave * 12 + 1
noteToInt (D , octave) = octave * 12 + 2
noteToInt (Ds, octave) = octave * 12 + 3
noteToInt (E , octave) = octave * 12 + 4
noteToInt (F , octave) = octave * 12 + 5
noteToInt (Fs, octave) = octave * 12 + 6
noteToInt (G , octave) = octave * 12 + 7
noteToInt (Gs, octave) = octave * 12 + 8
noteToInt (A , octave) = octave * 12 + 9
noteToInt (As, octave) = octave * 12 + 10
noteToInt (B , octave) = octave * 12 + 11

noteFreq :: Note -> Double
noteFreq note = 440.0 * (2.0 ** ((noteNumber - 57.0) / 12.0))
  where noteNumber = fromIntegral $ noteToInt note :: Double

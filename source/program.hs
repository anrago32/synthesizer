module Main where

import AudioGeneration
import Pitch

main :: IO ()
main = do
  let tone = generateAudio (C, 0)
  let times = [t | t <- [0..3000], t `mod` 750 == 0]
  let wave = map tone times
  mapM_ print wave

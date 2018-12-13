module Main where

import Sound.Pulse.Simple
import PitchConversion
import ToneGeneration

main :: IO ()
main = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  s <- simpleNew Nothing "nothing"
    Play Nothing "nothing"
    sampleSpec Nothing Nothing
  let times = [0..48000 * 4]
  let tone1 = generateTone (C, 5) times
  let tone2 = generateTone (E, 5) times
  let tone3 = generateTone (G, 5) times
  let audio = generateChord [tone1, tone2, tone3]
  simpleWrite s audio
  simpleDrain s
  simpleFree s

-- Audio
-- Description goes here
-- Written by Alex Rago, 2020

module Audio where

import Sound.Pulse.Simple
import Pitch

type AudioPlayer = Simple

data Envelope = Envelope
  { attack         :: Float
  , decay          :: Float
  , sustain        :: Float
  , release        :: Float
  , timeReleased   :: Float
  , volumeReleased :: Float
  }

createPlayer :: IO AudioPlayer
createPlayer = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  player <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  return player

oscPhase :: Pitch -> Int -> Float
oscPhase pitch time = fromIntegral time * frequency * pi / 24000
  where frequency = standardPitch pitch

sineOsc :: Pitch -> Int -> Float
sineOsc pitch time = sin phase * 0.2
  where phase = oscPhase pitch time

amOsc :: Pitch -> Int -> Float -> Float
amOsc pitch time m = sin phase * sin (phase * m) * 0.2
  where phase = oscPhase pitch time

fmOsc :: Pitch -> Int -> Float -> Float
fmOsc pitch time m = sin (sin phase * m) * 0.2
  where phase = sineOsc pitch time

-- Create attack, decay, sustain, release envelope
createEnvelope :: Int -> Int -> Int -> Int -> Envelope
createEnvelope a d s r = Envelope
  { attack = rescaleAttack a
  , decay = rescaleDecay d
  , sustain = rescaleSustain s
  , release = rescaleRelease r
  , timeReleased = -1
  , volumeReleased = -1
  }

rescaleAttack :: Int -> Float
rescaleAttack level = 1e-5 * fromIntegral level ^ 3 * 48000

rescaleDecay :: Int -> Float
rescaleDecay level = 1e-4 * fromIntegral level ^ 3 * 48000

rescaleSustain :: Int -> Float
rescaleSustain level = 1e-2 * fromIntegral level

rescaleRelease :: Int -> Float
rescaleRelease level = 1e-4 * fromIntegral level ^ 3 * 48000

-- Calculate envelope value at point in time
calculateEnvelope :: Envelope -> Int -> Float
calculateEnvelope envelope time
  | t < a = calculateAttack envelope t
  | t < a + d = calculateDecay envelope t
  | m == -1 = calculateSustain envelope t
  | t < m + r = calculateRelease envelope t
  | otherwise = 0
  where a = attack envelope
        d = decay envelope
        m = timeReleased envelope
        r = release envelope
        t = fromIntegral time

calculateAttack :: Envelope -> Float -> Float
calculateAttack (Envelope a d s _ _ _) t = 1 + (t - a) ^ 3 / a ^ 3

calculateDecay :: Envelope -> Float -> Float
calculateDecay (Envelope a d s _ _ _) t = s + (1 - s) * (a + d - t) ^ 3 / d ^ 3

calculateSustain :: Envelope -> Float -> Float
calculateSustain (Envelope a d s _ _ _) t = s

calculateRelease :: Envelope -> Float -> Float
calculateRelease (Envelope _ _ _ r m v) t = v * (m + r - t) ^ 3 / r ^ 3

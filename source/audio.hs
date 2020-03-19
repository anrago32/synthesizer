-- Audio
-- Description goes here
-- Written by Alex Rago, 2020

module Audio where

import Sound.Pulse.Simple
import Pitch

data Envelope = Envelope
  { attack         :: Float
  , decay          :: Float
  , sustain        :: Float
  , release        :: Float
  , timeReleased   :: Float
  , volumeReleased :: Float
  }

createPlayer :: IO Simple
createPlayer = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  player <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  return player

adjustedAttack :: Int -> Float
adjustedAttack level = 1e-5 * fromIntegral level ^ 3 * 48000

adjustedDecay :: Int -> Float
adjustedDecay level = 1e-4 * fromIntegral level ^ 3 * 48000

adjustedSustain :: Int -> Float
adjustedSustain level = 1e-2 * fromIntegral level

adjustedRelease :: Int -> Float
adjustedRelease level = 1e-4 * fromIntegral level ^ 3 * 48000

createEnvelope :: Int -> Int -> Int -> Int -> Envelope
createEnvelope a d s r = Envelope
  { attack = adjustedAttack a
  , decay = adjustedDecay d
  , sustain = adjustedSustain s
  , release = adjustedRelease r
  , timeReleased = -1
  , volumeReleased = -1
  }

calculateAttack :: Envelope -> Float -> Float
calculateAttack (Envelope a d s _ _ _) t = 1 + (t - a) ^ 3 / a ^ 3

calculateDecay :: Envelope -> Float -> Float
calculateDecay (Envelope a d s _ _ _) t = s + (1 - s) * (a + d - t) ^ 3 / d ^ 3

calculateSustain :: Envelope -> Float -> Float
calculateSustain (Envelope a d s _ _ _) t = s

calculateRelease :: Envelope -> Float -> Float
calculateRelease (Envelope _ _ _ r m v) t = v * (m + r - t) ^ 3 / r ^ 3

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

sineOsc :: Pitch -> Int -> Float
sineOsc pitch time = sin phase * 0.2
  where phase = fromIntegral time * frequency * pi * 2.0 / sampleRate
        frequency = standardPitch pitch
        sampleRate = 48000.0

-- FM Oscillator Prototype
-- fmOsc :: Pitch -> Int -> Float
-- fmOsc pitch time = sin (sineOsc pitch time * 2) * 0.2

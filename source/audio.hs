-- Audio
-- Description goes here
-- Written by Alex Rago, 2020

module Audio where

import Sound.Pulse.Simple
import Pitch

createPlayer :: IO Simple
createPlayer = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  player <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  return player

playChord :: [Float] -> Float
playChord tones = sum tones

data Envelope = Envelope
  { attack         :: Float
  , decay          :: Float
  , sustain        :: Float
  , release        :: Float
  , timeReleased   :: Float
  , volumeReleased :: Float
  }

adjustedEnvelopeTime :: Int -> Float
adjustedEnvelopeTime time = 25e-8 * fromIntegral time ^ 4 * 48000

adjustedEnvelopeLevel :: Int -> Float
adjustedEnvelopeLevel level = 1e-2 * fromIntegral level

newEnvelope :: Int -> Int -> Int -> Int -> Envelope
newEnvelope a d s r = Envelope
  { attack = adjustedEnvelopeTime a
  , decay = adjustedEnvelopeTime d
  , sustain = adjustedEnvelopeLevel s
  , release = adjustedEnvelopeTime r
  , timeReleased = -1
  , volumeReleased = -1
  }

calculateAttack :: Envelope -> Float -> Float
calculateAttack (Envelope a d s _ _ _) t = t / a

calculateDecay :: Envelope -> Float -> Float
calculateDecay (Envelope a d s _ _ _) t = 1 + (1 - s) * (a - t) / d

calculateSustain :: Envelope -> Float -> Float
calculateSustain (Envelope a d s _ _ _) t = s

calculateRelease :: Envelope -> Float -> Float
calculateRelease (Envelope _ _ _ r m v) t = v + v * (m - t) / r

envelopeLevel :: Envelope -> Int -> Float
envelopeLevel envelope time
  | t < a = calculateAttack envelope t ** (-shape)
  | t < a + d = calculateDecay envelope t ** shape
  | m == -1 = calculateSustain envelope t ** shape
  | t < m + r = calculateRelease envelope t ** shape
  | otherwise = 0
  where a = attack envelope
        d = decay envelope
        m = timeReleased envelope
        r = release envelope
        t = fromIntegral time
        shape = log $ t / 48000

sineOsc :: Pitch -> Int -> Float
sineOsc pitch time = sin phase * 0.2
  where phase = fromIntegral time * frequency * pi * 2.0 / sampleRate
        frequency = standardPitch pitch
        sampleRate = 48000.0

-- FM Oscillator Prototype
-- fmOsc :: Pitch -> Int -> Float
-- fmOsc pitch time = sin (sineOsc pitch time * 2) * 0.2

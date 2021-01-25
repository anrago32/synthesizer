module Audio where

import Sound.Pulse.Simple
import Pitch

data Envelope = Envelope
  { attack         :: Float
  , decay          :: Float
  , sustain        :: Float
  , release        :: Float
  }

data Note = Note
  { timeElapsed    :: Float
  , timeReleased   :: Float
  , volumeReleased :: Float
  , pitch          :: Pitch
  }

instance Eq Note where
  a == b = pitch a == pitch b

instance Ord Note where
  a <= b = pitch a <= pitch b

-- Data Constructors
envelopeNew :: Int -> Int -> Int -> Int -> Envelope
envelopeNew a d s r = Envelope
  { attack = rescaleAttack a
  , decay = rescaleDecay d
  , sustain = rescaleSustain s
  , release = rescaleRelease r
  }

noteNew :: Pitch -> Note
noteNew p = Note
  { timeElapsed = 0
  , timeReleased = -1
  , volumeReleased = -1
  , pitch = p
  }

-- Control Rescaling
rescaleAttack :: Int -> Float
rescaleAttack level = 1e-5 * fromIntegral level ^ 3 * 48000

rescaleDecay :: Int -> Float
rescaleDecay level = 1e-4 * fromIntegral level ^ 3 * 48000

rescaleSustain :: Int -> Float
rescaleSustain level = 1e-2 * fromIntegral level

rescaleRelease :: Int -> Float
rescaleRelease level = 1e-4 * fromIntegral level ^ 3 * 48000

-- Envelope Generation
generateEnvelope :: Envelope -> Note -> Float
generateEnvelope envelope note
  | m /= -1 = calculateRelease envelope note
  | t < a = calculateAttack envelope note
  | t < a + d = calculateDecay envelope note
  | otherwise = calculateSustain envelope note
  where (Envelope a d _ _) = envelope
        (Note t m _ _) = note

calculateAttack :: Envelope -> Note -> Float
calculateAttack envelope note = 1 + (t - a) ^ 3 / a ^ 3
  where (Envelope a _ _ _) = envelope
        (Note t _ _ _) = note

calculateDecay :: Envelope -> Note -> Float
calculateDecay envelope note = s + (1 - s) * (a + d - t) ^ 3 / d ^ 3
  where (Envelope a d s _) = envelope
        (Note t _ _ _) = note

calculateSustain :: Envelope -> Note -> Float
calculateSustain envelope note = s
  where (Envelope _ _ s _) = envelope
        (Note _ _ _ _) = note

calculateRelease :: Envelope -> Note -> Float
calculateRelease envelope note = v * (m + r - t) ^ 3 / r ^ 3
  where (Envelope _ _ _ r) = envelope
        (Note t m v _) = note

-- Oscillator Generation
amOscillator :: Note -> Float -> Float
amOscillator note m = sin phase * sin (phase * m) * 0.2
  where phase = oscillatorPhase note

fmOscillator :: Note -> Float -> Float
fmOscillator note m = sin (sin phase * m) * 0.2
  where phase = oscillatorPhase note

sineOscillator :: Note -> Float
sineOscillator note = sin phase * 0.2
  where phase = oscillatorPhase note

oscillatorPhase :: Note -> Float
oscillatorPhase note = time * frequency * pi / 24000
  where frequency = standardPitch $ pitch note
        time = timeElapsed note

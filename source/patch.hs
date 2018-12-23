module Patch where

data Patch = Patch
  { volume          :: Int
  , octave          :: Int
  , portamento      :: Int
  , envelopeMod     :: Int
  , envelopeAttack  :: Int
  , envelopeDecay   :: Int
  , envelopeSustain :: Int
  , envelopeRelease :: Int
  , oscillatorType  :: String
  , oscillatorMod1  :: Int
  , oscillatorMod2  :: Int
  , filterType      :: String
  , filterCutoff    :: Int
  , filterResonance :: Int
  , lfoType         :: String
  , lfoDepth        :: Int
  , lfoRate         :: Int
  , effectType      :: String
  , effectDepth     :: Int
  , effectRate      :: Int
  }

-- loadPatch :: String -> IO Patch

-- savePatch :: String -> Patch -> IO ()

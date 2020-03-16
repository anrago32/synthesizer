-- Patch
-- Description goes here
-- Written by Alex Rago, 2020

module Patch where

data Patch = Patch
  { oscillatorType :: Int
  , filterType     :: Int
  , lfoType        :: Int
  , effectType     :: Int
  , volume         :: Double
  , octave         :: Double
  , glissando      :: Double
  , envelope       :: Double
  , attack         :: Double
  , decay          :: Double
  , sustain        :: Double
  , release        :: Double
  , modulation     :: Double
  , texture        :: Double
  , cutoff         :: Double
  , resonance      :: Double
  , lfoDepth       :: Double
  , lfoRate        :: Double
  , effectDepth    :: Double
  , effectRate     :: Double
  } deriving (Read)

instance Show Patch where
  show patch = "Patch\n"
    ++ "  { oscillatorType = " ++ show (oscillatorType patch) ++ "\n"
    ++ "  , filterType     = " ++ show (filterType     patch) ++ "\n"
    ++ "  , lfoType        = " ++ show (lfoType        patch) ++ "\n"
    ++ "  , effectType     = " ++ show (effectType     patch) ++ "\n"
    ++ "  , volume         = " ++ show (volume         patch) ++ "\n"
    ++ "  , octave         = " ++ show (octave         patch) ++ "\n"
    ++ "  , glissando      = " ++ show (glissando      patch) ++ "\n"
    ++ "  , envelope       = " ++ show (envelope       patch) ++ "\n"
    ++ "  , attack         = " ++ show (attack         patch) ++ "\n"
    ++ "  , decay          = " ++ show (decay          patch) ++ "\n"
    ++ "  , sustain        = " ++ show (sustain        patch) ++ "\n"
    ++ "  , release        = " ++ show (release        patch) ++ "\n"
    ++ "  , modulation     = " ++ show (modulation     patch) ++ "\n"
    ++ "  , texture        = " ++ show (texture        patch) ++ "\n"
    ++ "  , cutoff         = " ++ show (cutoff         patch) ++ "\n"
    ++ "  , resonance      = " ++ show (resonance      patch) ++ "\n"
    ++ "  , lfoDepth       = " ++ show (lfoDepth       patch) ++ "\n"
    ++ "  , lfoRate        = " ++ show (lfoRate        patch) ++ "\n"
    ++ "  , effectDepth    = " ++ show (effectDepth    patch) ++ "\n"
    ++ "  , effectRate     = " ++ show (effectRate     patch) ++ "\n"
    ++ "  }\n"

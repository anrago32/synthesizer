-- Patch
-- Module Containing Patch Data Type
-- Written by Alex Rago, 2020

module Patch where

data Patch = Patch
  { oscillatorType   :: Int
  , filterType       :: Int
  , lfoType          :: Int
  , effectType       :: Int
  , volumeLevel      :: Double
  , envelopeLevel    :: Double
  , attackLevel      :: Double
  , decayLevel       :: Double
  , sustainLevel     :: Double
  , releaseLevel     :: Double
  , modulationLevel  :: Double
  , textureLevel     :: Double
  , cutoffLevel      :: Double
  , resonanceLevel   :: Double
  , lfoDepthLevel    :: Double
  , lfoRateLevel     :: Double
  , effectDepthLevel :: Double
  , effectRateLevel  :: Double
  } deriving (Read)

instance Show Patch where
  show patch = "Patch\n"
    ++ "  { oscillatorType   = " ++ show (oscillatorType   patch) ++ "\n"
    ++ "  , filterType       = " ++ show (filterType       patch) ++ "\n"
    ++ "  , lfoType          = " ++ show (lfoType          patch) ++ "\n"
    ++ "  , effectType       = " ++ show (effectType       patch) ++ "\n"
    ++ "  , volumeLevel      = " ++ show (volumeLevel      patch) ++ "\n"
    ++ "  , envelopeLevel    = " ++ show (envelopeLevel    patch) ++ "\n"
    ++ "  , attackLevel      = " ++ show (attackLevel      patch) ++ "\n"
    ++ "  , decayLevel       = " ++ show (decayLevel       patch) ++ "\n"
    ++ "  , sustainLevel     = " ++ show (sustainLevel     patch) ++ "\n"
    ++ "  , releaseLevel     = " ++ show (releaseLevel     patch) ++ "\n"
    ++ "  , modulationLevel  = " ++ show (modulationLevel  patch) ++ "\n"
    ++ "  , textureLevel     = " ++ show (textureLevel     patch) ++ "\n"
    ++ "  , cutoffLevel      = " ++ show (cutoffLevel      patch) ++ "\n"
    ++ "  , resonanceLevel   = " ++ show (resonanceLevel   patch) ++ "\n"
    ++ "  , lfoDepthLevel    = " ++ show (lfoDepthLevel    patch) ++ "\n"
    ++ "  , lfoRateLevel     = " ++ show (lfoRateLevel     patch) ++ "\n"
    ++ "  , effectDepthLevel = " ++ show (effectDepthLevel patch) ++ "\n"
    ++ "  , effectRateLevel  = " ++ show (effectRateLevel  patch) ++ "\n"
    ++ "  }\n"

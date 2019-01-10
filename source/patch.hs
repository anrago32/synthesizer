module Patch where

data Patch = Patch
  { volume          :: Int
  , octave          :: Int
  , glissando       :: Int
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
  } deriving (Read, Show)

{-
loadPatch :: String -> IO Patch
loadPatch file = do
  contents <- readFile file
  patch <- read contents
  return patch

savePatch :: String -> Patch -> IO ()
savePatch file patch = do
  let contents = show patch
  writeFile file contents
-}

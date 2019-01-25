module Patch where

data Patch = Patch
  { effect     :: String
  , filter     :: String
  , lfo        :: String
  , oscillator :: String
  , attack     :: Double
  , cutoff     :: Double
  , decay      :: Double
  , effDepth   :: Double
  , effRate    :: Double
  , envelope   :: Double
  , glissando  :: Double
  , lfoDepth   :: Double
  , lfoRate    :: Double
  , modulation :: Double
  , octave     :: Double
  , release    :: Double
  , resonance  :: Double
  , sustain    :: Double
  , texture    :: Double
  , volume     :: Double
  } deriving (Read, Show)

loadPatch :: String -> IO ()
loadPatch file = do
  contents <- readFile file
  let patch = read contents
  return patch

savePatch :: String -> Patch -> IO ()
savePatch file patch = do
  let contents = show patch
  writeFile file contents

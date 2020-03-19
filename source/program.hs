-- Synthesizer
-- Audio synthesizer written in Haskell
-- Written by Alex Rago, 2020

module Main where

import Control.Concurrent
import Data.IORef as IORef
import Data.List as List
import Data.Map as Map
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Sound.Pulse.Simple
import Audio
import GUI
import Patch
import Pitch

type KeyboardState = IORef [Note]

main :: IO ()
main = do
  initGUI
  gui <- newGUI
  audioPlayer <- newPlayer
  keyboardState <- newIORef []

  setPatch gui "/home/anrago/code/synthesizer/files/default.patch"
  onClicked (adjustButton gui) (adjustSettings gui)
  onClicked (loadButton gui) (loadPatch gui)
  onClicked (saveButton gui) (savePatch gui)
  onKeyPress (mainWindow gui) (keyboardHandler audioPlayer keyboardState)
  onKeyRelease (mainWindow gui) (keyboardHandler audioPlayer keyboardState)

  mainGUI
  simpleFree audioPlayer

pitchMap :: Map Char Pitch
pitchMap = fromList
  [ ('z', (C, 4)), ('s', (Cs, 4)), ('x', (D, 4)), ('d', (Ds, 4)), ('c', (E, 4))
  , ('v', (F, 4)), ('g', (Fs, 4)), ('b', (G, 4)), ('h', (Gs, 4)), ('n', (A, 4))
  , ('j', (As, 4)), ('m', (B, 4)), (',', (C, 5)), ('l', (Cs, 5)), ('.', (D, 5))
  , (';', (Ds, 5)), ('/', (E, 5)), ('q', (C, 5)), ('2', (Cs, 5)), ('w', (D, 5))
  , ('3', (Ds, 5)), ('e', (E, 5)), ('r', (F, 5)), ('5', (Fs, 5)), ('t', (G, 5))
  , ('6', (Gs, 5)), ('y', (A, 5)), ('7', (As, 5)), ('u', (B, 5)), ('i', (C, 6))
  , ('9', (Cs, 6)), ('o', (D, 6)), ('0', (Ds, 6)), ('p', (E, 6)), ('[', (F, 6))
  , ('=', (Fs, 6)), (']', (G, 6)), ('\\', (A, 6))
  ]

keyboardHandler :: AudioPlayer -> KeyboardState -> Event -> IO Bool
keyboardHandler audioPlayer keyboardState event = case event of
    (Key released _ _ _ _ _ _ _ _ (Just character)) ->
      if member character pitchMap
        then do
          notes <- readIORef keyboardState
          let pitch = pitchMap ! character
          if released
            then handleKeyRelease character keyboardState
            else handleKeyPress audioPlayer character keyboardState
          return True
        else return False
    _ -> return False

sameNote :: Pitch -> Note -> Bool
sameNote p note = p == pitch note

append :: Note -> [Note] -> [Note]
append new list = list ++ [new]

replace :: Note -> Note -> [Note] -> [Note]
replace old new list = append new $ List.filter (\x -> (not $ sameNote (pitch x) old)) list

incrementTime :: Note -> Note
incrementTime note = note {timeElapsed = t}
  where t = timeElapsed note + 1

incrementTimes :: [Note] -> [Note]
incrementTimes notes = incrementTime <$> notes

updateVolume :: Float -> Note -> Note
updateVolume volume note = if timeReleased note == -1 then note {volumeReleased = volume} else note

updateVolumes :: [Float] -> [Note] -> [Note]
updateVolumes volumes notes = zipWith (updateVolume) volumes notes

handleKeyPress :: AudioPlayer -> Char -> KeyboardState -> IO ()
handleKeyPress audioPlayer character keyboardState = do
  notes <- readIORef keyboardState
  let i = List.findIndex (sameNote $ pitchMap ! character) notes
  case i of
    (Just index) -> return ()
    Nothing -> do
      let new = newNote $ pitchMap ! character
      modifyIORef keyboardState (append new)
      if length notes == 0
        then (forkIO $ playAudio audioPlayer keyboardState) >>= (\a -> return ())
        else (forkIO $ return ()) >>= (\a -> return ())

      notes <- readIORef keyboardState
      print notes

handleKeyRelease :: Char -> KeyboardState -> IO ()
handleKeyRelease character keyboardState = do
  notes <- readIORef keyboardState
  let i = List.findIndex (sameNote $ pitchMap ! character) notes
  case i of
    Nothing -> return ()
    Just index -> do
      let oldNote = notes !! index
      let t = timeElapsed oldNote
      let new = oldNote {timeReleased = t}
      modifyIORef keyboardState (replace oldNote new)

playAudio :: AudioPlayer -> KeyboardState -> IO ()
playAudio audioPlayer keyboardState = do
  notes <- readIORef keyboardState
  if length notes /= 0
    then do
      let envelope = newEnvelope 10 10 90 25
      let oscillatorValues = sineOscillator <$> notes
      let envelopeValues = calculateEnvelope envelope <$> notes
      let sample = sum $ zipWith (*) oscillatorValues envelopeValues
      simpleWrite audioPlayer $ [sample]
      modifyIORef keyboardState (incrementTimes)
      modifyIORef keyboardState (updateVolumes envelopeValues)
      playAudio audioPlayer keyboardState
    else return ()

newPlayer :: IO AudioPlayer
newPlayer = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  player <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  return player

-- Synthesizer
-- Audio synthesizer written in Haskell
-- Written by Alex Rago, 2020

module Main where

import Control.Concurrent
import Data.IORef as IORef
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Sound.Pulse.Simple
import Audio
import GUI
import Patch
import Pitch

type State = IORef [Note]

stateNew :: IO State
stateNew = newIORef []

stateCreateNote :: State -> Pitch -> IO ()
stateCreateNote state p = do
  notes <- readIORef state
  let newNote = noteNew p
  let newNotes = List.insert newNote notes
  writeIORef state newNotes

stateIncrementTime :: State -> IO ()
stateIncrementTime state = do
  notes <- readIORef state
  let increment note = note {
    timeElapsed = timeElapsed note + 1}
  let newNotes = increment <$> notes
  writeIORef state newNotes

stateReleaseNote :: State -> Envelope -> Pitch -> IO ()
stateReleaseNote state envelope p = do
  notes <- readIORef state
  let note = fromJust $ find (\x -> pitch x == p) notes
  let newNote = note {
    timeReleased = timeElapsed note,
    volumeReleased = calculateEnvelope envelope note}
  let newNotes = List.insert newNote . List.delete note $ notes
  writeIORef state newNotes

stateClean :: State -> Envelope -> IO ()
stateClean state envelope = do
  notes <- readIORef state
  let live n = calculateEnvelope envelope n > 0
  let newNotes = List.filter live notes
  writeIORef state newNotes

main :: IO ()
main = do
  initGUI
  gui <- guiNew
  audioPlayer <- playerNew
  keyboardState <- stateNew

  setPatch gui "/home/anrago/code/synthesizer/files/default.patch"
  onClicked (adjustButton gui) (adjustSettings gui)
  onClicked (loadButton gui) (loadPatch gui)
  onClicked (saveButton gui) (savePatch gui)
  onKeyPress (mainWindow gui) (keyboardHandler audioPlayer keyboardState)
  onKeyRelease (mainWindow gui) (keyboardHandler audioPlayer keyboardState)

  mainGUI
  simpleFree audioPlayer

pitchMap :: Map Char Pitch
pitchMap = Map.fromList
  [ ('z', (C, 4)), ('s', (Cs, 4)), ('x', (D, 4)), ('d', (Ds, 4)), ('c', (E, 4))
  , ('v', (F, 4)), ('g', (Fs, 4)), ('b', (G, 4)), ('h', (Gs, 4)), ('n', (A, 4))
  , ('j', (As, 4)), ('m', (B, 4)), (',', (C, 5)), ('l', (Cs, 5)), ('.', (D, 5))
  , (';', (Ds, 5)), ('/', (E, 5)), ('q', (C, 5)), ('2', (Cs, 5)), ('w', (D, 5))
  , ('3', (Ds, 5)), ('e', (E, 5)), ('r', (F, 5)), ('5', (Fs, 5)), ('t', (G, 5))
  , ('6', (Gs, 5)), ('y', (A, 5)), ('7', (As, 5)), ('u', (B, 5)), ('i', (C, 6))
  , ('9', (Cs, 6)), ('o', (D, 6)), ('0', (Ds, 6)), ('p', (E, 6)), ('[', (F, 6))
  , ('=', (Fs, 6)), (']', (G, 6)), ('\\', (A, 6))
  ]

keyboardHandler :: AudioPlayer -> State -> Event -> IO Bool
keyboardHandler audioPlayer keyboardState event = case event of
    (Key released _ _ _ _ _ _ _ _ (Just character)) ->
      if Map.member character pitchMap
        then if released
          then handleKeyRelease character keyboardState >> return True
          else handleKeyPress audioPlayer character keyboardState >> return True
        else return False
    _ -> return False

sameNote :: Pitch -> Note -> Bool
sameNote p note = p == pitch note

handleKeyPress :: AudioPlayer -> Char -> State -> IO ()
handleKeyPress audioPlayer character keyboardState = do
  notes <- readIORef keyboardState
  let i = List.findIndex (sameNote $ pitchMap ! character) notes
  case i of
    (Just index) -> return ()
    Nothing -> do
      stateCreateNote keyboardState $ pitchMap ! character
      if length notes == 0
        then (forkIO $ playAudio audioPlayer keyboardState) >>= (\a -> return ())
        else (forkIO $ return ()) >>= (\a -> return ())

handleKeyRelease :: Char -> State -> IO ()
handleKeyRelease character keyboardState = do
  notes <- readIORef keyboardState
  let i = List.findIndex (sameNote $ pitchMap ! character) notes
  case i of
    Nothing -> return ()
    Just index -> do
      let envelope = envelopeNew 10 25 75 10
      stateReleaseNote keyboardState envelope $ pitchMap ! character

playAudio :: AudioPlayer -> State -> IO ()
playAudio audioPlayer keyboardState = do
  notes <- readIORef keyboardState
  if length notes /= 0
    then do
      let envelope = envelopeNew 10 25 75 10
      let oscillatorValues = sineOscillator <$> notes
      let envelopeValues = calculateEnvelope envelope <$> notes
      let sample = sum $ zipWith (*) oscillatorValues envelopeValues
      simpleWrite audioPlayer $ [sample]
      stateIncrementTime keyboardState
      stateClean keyboardState envelope
      playAudio audioPlayer keyboardState
    else return ()

playerNew :: IO AudioPlayer
playerNew = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  player <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  return player

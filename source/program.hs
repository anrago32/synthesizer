-- Synthesizer
-- Audio synthesizer written in Haskell
-- Written by Alex Rago, 2020

module Main where

import Audio
import Control.Concurrent
import Data.IORef
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Text
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Events
import GUI
import Patch
import Pitch
import Sound.Pulse.Simple

type Player = Simple
type State = IORef [Note]

main :: IO ()
main = do
  initGUI
  gui <- guiNew
  player <- playerNew
  state <- stateNew

  let envelope = envelopeNew 10 25 75 25

  forkIO $ playAudio state envelope player
  onClicked (adjustButton gui) (adjustSettings gui)
  onClicked (loadButton gui) (loadPatch gui)
  onClicked (saveButton gui) (savePatch gui)
  onKeyPress (mainWindow gui) (handleKeyEvent state envelope)
  onKeyRelease (mainWindow gui) (handleKeyEvent state envelope)
  setPatch gui "/home/anrago/code/synthesizer/files/default.patch"

  mainGUI
  simpleFree player

-- Audio Output
playAudio :: State -> Envelope -> Player -> IO ()
playAudio state envelope player = do
  notes <- stateRead state
  let oscillatorValues = sineOscillator <$> notes
  let envelopeValues = generateEnvelope envelope <$> notes
  let sample = sum $ List.zipWith (*) oscillatorValues envelopeValues
  simpleWrite player [sample]; stateIncrementTime state
  stateClearDead state envelope; playAudio state envelope player

playerNew :: IO Player
playerNew = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  player <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  return player

-- Event Handling
handleKeyEvent :: State -> Envelope -> Event -> IO Bool
handleKeyEvent state envelope event = do
  let key = unpack $ Events.eventKeyName event
  if member key keyPitchMap && not (eventRelease event)
  then handleKeyPress state envelope $ keyPitchMap ! key
  else if member key keyPitchMap && eventRelease event
  then handleKeyRelease state envelope $ keyPitchMap ! key
  else return (); return True

handleKeyPress :: State -> Envelope -> Pitch -> IO ()
handleKeyPress state envelope p = do
  notes <- stateRead state
  if elem p (pitch <$> notes) then do
    let note = fromJust $ List.find ((== p) . pitch) notes
    if timeReleased note /= -1
    then stateBeginNote state p
    else return ()
  else stateBeginNote state p

handleKeyRelease :: State -> Envelope -> Pitch -> IO ()
handleKeyRelease state envelope p = do
  notes <- stateRead state
  if elem p (pitch <$> notes)
  then stateReleaseNote state envelope p
  else return ()

keyPitchMap :: Map String Pitch
keyPitchMap = Map.fromList
  [ ("Shift_L", (C, 4)), ("A", (Cs, 4)), ("a", (Cs, 4)), ("Z", (D, 4))
  , ("z", (D, 4)), ("S", (Ds, 4)), ("s", (Ds, 4)), ("X", (E, 4)), ("x", (E, 4))
  , ("C", (F, 4)), ("c", (F, 4)), ("F", (Fs, 4)), ("f", (Fs, 4)), ("V", (G, 4))
  , ("v", (G, 4)), ("G", (Gs, 4)), ("g", (Gs, 4)), ("B", (A, 4)), ("b", (A, 4))
  , ("H", (As, 4)), ("h", (As, 4)), ("N", (B, 4)), ("n", (B, 4)), ("M", (C, 5))
  , ("m", (C, 5)), ("K", (Cs, 5)), ("k", (Cs, 5)), ("less", (D, 5))
  , ("comma", (D, 5)), ("L", (Ds, 5)), ("l", (Ds, 5)), ("greater", (E, 5))
  , ("period", (E, 5)), ("question", (F, 5)), ("slash", (F, 5))
  , ("quotedbl", (Fs, 5)), ("apostrophe", (Fs, 5)), ("Shift_R", (G, 5))
  ]

-- State Operations
stateNew :: IO State
stateNew = newIORef []

stateRead :: State -> IO [Note]
stateRead state = readIORef state

stateBeginNote :: State -> Pitch -> IO ()
stateBeginNote state p = do
  notes <- readIORef state
  let newNote = noteNew p
  let newNotes = List.insert newNote notes
  writeIORef state newNotes

stateClearDead :: State -> Envelope -> IO ()
stateClearDead state envelope = do
  notes <- readIORef state
  let live n = generateEnvelope envelope n > 0
  let newNotes = List.filter live notes
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
  let note = fromJust $ List.find ((== p) . pitch) notes
  let newNote = note {
    timeReleased = timeElapsed note,
    volumeReleased = generateEnvelope envelope note}
  let newNotes = List.insert newNote . List.delete note $ notes
  writeIORef state newNotes

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
  setPatch gui "/home/anrago/code/synthesizer/files/default.patch"
  onClicked (adjustButton gui) (adjustSettings gui)
  onClicked (loadButton gui) (loadPatch gui)
  onClicked (saveButton gui) (savePatch gui)
  onKeyPress (mainWindow gui) (handleKeyEvent state envelope)
  onKeyRelease (mainWindow gui) (handleKeyEvent state envelope)

  mainGUI
  simpleFree player

-- Audio Output
playAudio :: State -> Envelope -> Player -> IO ()
playAudio state envelope player = do
  notes <- stateRead state
  let oscillatorValues = sineOscillator <$> notes
  let envelopeValues = calculateEnvelope envelope <$> notes
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
  if member key pitchMap && not (eventRelease event)
  then handleKeyPress state envelope $ pitchMap ! key
  else if member key pitchMap && eventRelease event
  then handleKeyRelease state envelope $ pitchMap ! key
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

pitchMap :: Map String Pitch
pitchMap = Map.fromList
  [ ("z", (C, 4)), ("s", (Cs, 4)), ("x", (D, 4)), ("d", (Ds, 4)), ("c", (E, 4))
  , ("v", (F, 4)), ("g", (Fs, 4)), ("b", (G, 4)), ("h", (Gs, 4)), ("n", (A, 4))
  , ("j", (As, 4)), ("m", (B, 4)), (",", (C, 5)), ("l", (Cs, 5)), (".", (D, 5))
  , (";", (Ds, 5)), ("/", (E, 5)), ("q", (C, 5)), ("2", (Cs, 5)), ("w", (D, 5))
  , ("3", (Ds, 5)), ("e", (E, 5)), ("r", (F, 5)), ("5", (Fs, 5)), ("t", (G, 5))
  , ("6", (Gs, 5)), ("y", (A, 5)), ("7", (As, 5)), ("u", (B, 5)), ("i", (C, 6))
  , ("9", (Cs, 6)), ("o", (D, 6)), ("0", (Ds, 6)), ("p", (E, 6)), ("[", (F, 6))
  , ("=", (Fs, 6)), ("]", (G, 6)), ("\\", (A, 6))
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
  let live n = calculateEnvelope envelope n > 0
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
    volumeReleased = calculateEnvelope envelope note}
  let newNotes = List.insert newNote . List.delete note $ notes
  writeIORef state newNotes

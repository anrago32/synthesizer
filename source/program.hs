-- Synthesizer
-- Audio synthesizer written in Haskell
-- Written by Alex Rago, 2020

module Main where

import Control.Concurrent
import Data.IORef
import Data.Map as Map
import Data.Set as Set
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Sound.Pulse.Simple
import Audio
import Gui
import Patch
import Pitch

type AudioPlayer = Simple
type KeyboardState = IORef (Set Pitch)

main :: IO ()
main = do
  initGUI
  gui <- createGui
  audioPlayer <- createPlayer
  keyboardState <- newIORef $ Set.fromList []

  setPatch gui "/home/anrago/code/synthesizer/files/default.patch"
  onClicked (configureButton gui) (configuration gui)
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

keyboardHandler :: AudioPlayer -> KeyboardState -> Event -> IO Bool
keyboardHandler audioPlayer keyboardState event = case event of
    (Key released _ _ _ _ _ _ _ _ (Just character)) ->
      if Map.member character pitchMap then do
        p <- readIORef keyboardState
        let pitch = pitchMap ! character
        if released then
          modifyIORef keyboardState (Set.delete pitch)
          else modifyIORef keyboardState (Set.insert pitch)
        if Set.size p == 0 then
          forkIO $ playAudio audioPlayer keyboardState 0
          else forkIO $ return ()
        pitches <- readIORef keyboardState
        return True
      else return False
    _ -> return False

playAudio :: AudioPlayer -> KeyboardState -> Int -> IO ()
playAudio audioPlayer keyboardState time = do
  p <- readIORef keyboardState
  let pitches = Set.toList p
  if length pitches /= 0 then do
    let sample = sum $ (\x -> sineOsc x time) <$> pitches

    let envelope = createEnvelope 25 50 75 50
    let shapedSample = calculateEnvelope envelope time * sample

    simpleWrite audioPlayer $ [shapedSample]
    playAudio audioPlayer keyboardState $ time + 1
    else return ()

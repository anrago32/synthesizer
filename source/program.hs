-- Synthesizer
-- Audio synthesizer written in Haskell
-- Written by Alex Rago, 2020

module Main where

import Control.Concurrent
import Data.IORef
import Data.Map
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Sound.Pulse.Simple
import Audio
import Gui
import Patch
import Pitch

type AudioPlayer = Simple
type KeyboardState = IORef [Pitch]

main :: IO ()
main = do
  initGUI
  gui <- createGui
  audioPlayer <- createPlayer
  keyboardState <- newIORef []

  setPatch gui "/home/anrago/Code/synthesizer/files/default.patch"
  onClicked (configureButton gui) (configuration gui)
  onClicked (loadButton gui) (loadPatch gui)
  onClicked (saveButton gui) (savePatch gui)
  onKeyPress (mainWindow gui) (keyboardHandler audioPlayer keyboardState)
  onKeyRelease (mainWindow gui) (keyboardHandler audioPlayer keyboardState)

  mainGUI
  simpleFree audioPlayer

keyPitchMap :: Map Char Pitch
keyPitchMap = fromList
  [ ('z', (C, 4)), ('s', (Cs, 4)), ('x', (D, 4)), ('d', (Ds, 4)), ('c', (E, 4))
  , ('v', (F, 4)), ('g', (Fs, 4)), ('b', (G, 4)), ('h', (Gs, 4)), ('n', (A, 4))
  , ('j', (As, 4)), ('m', (B, 4)), (',', (C, 5)), ('l', (Cs, 5)), ('.', (D, 5))
  , (';', (Ds, 5)), ('/', (E, 5)), ('q', (C, 6)), ('2', (Cs, 6)), ('w', (D, 6))
  , ('3', (Ds, 6)), ('e', (E, 6)), ('r', (F, 6)), ('5', (Fs, 6)), ('t', (G, 6))
  , ('6', (Gs, 6)), ('y', (A, 6)), ('7', (As, 6)), ('u', (B, 6)), ('i', (C, 7))
  , ('9', (Cs, 7)), ('o', (D, 7)), ('0', (Ds, 7)), ('p', (E, 7)), ('[', (F, 7))
  , ('=', (Fs, 7)), (']', (G, 7)), ('\\', (A, 7))
  ]

keyboardHandler :: AudioPlayer -> KeyboardState -> Event -> IO Bool
keyboardHandler audioPlayer keyboardState event = case event of
    (Key released _ _ _ _ _ _ _ _ (Just character)) ->
      if member character keyPitchMap then do
        let pitch = keyPitchMap ! character
        if released then
          modifyIORef keyboardState (Prelude.filter (/= pitch))
          else modifyIORef keyboardState (++ [pitch])
        forkIO $ playAudio audioPlayer keyboardState 0
        return True
      else return False
    _ -> return False

playAudio :: AudioPlayer -> KeyboardState -> Int -> IO ()
playAudio audioPlayer keyboardState time = do
  pitches <- readIORef keyboardState
  if length pitches /= 0 then do
    let pitch = pitches !! 0
    let audio = fmOsc pitch <$> [time..time + 8000]
    simpleWrite audioPlayer audio
    playAudio audioPlayer keyboardState $ time + 8000
    else return ()

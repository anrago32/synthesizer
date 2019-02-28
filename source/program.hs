module Main where

import Graphics.UI.Gtk
import Sound.Pulse.Simple
import Audio
import Gui
import Patch
import Pitch

main :: IO ()
main = do
  initGUI
  gui <- createGui
  setPatch gui "/home/anrago/Code/synthesizer/files/default.patch"
  onClicked (scriptButton gui) (processScript gui)
  onClicked (loadButton gui) (loadPatch gui)
  onClicked (saveButton gui) (savePatch gui)
  mainGUI

playAudio :: [Float] -> IO ()
playAudio audio = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  s <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  simpleWrite s audio
  simpleFree s

playNote :: IO ()
playNote = do
  let times = [0..48000 * 2]
  let audio = finalOutput <$> times
  playAudio audio

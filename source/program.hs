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
  window <- windowNew
  set window
    [ windowTitle := "Synthesizer"
    , windowResizable := False
    ]
  gui <- createGui window
  widgetShowAll window
  mainGUI

playAudio :: [Float] -> IO ()
playAudio audio = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  s <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  simpleWrite s audio
  simpleDrain s
  simpleFree s

playNote :: IO ()
playNote = do
  let times = [0..48000 * 2]
  let audio = map finalOutput times
  playAudio audio

module Main where

import Graphics.UI.Gtk
import Sound.Pulse.Simple
import AudioGeneration

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "Synthesizer"
    , containerBorderWidth := 20
    ]

  button <- buttonNew
  button `onClicked` playChord
  box <- labelBox "Play Chord"
  button `containerAdd` box
  window `containerAdd` button
  widgetShowAll window
  window `onDestroy` mainQuit
  mainGUI

labelBox :: String -> IO HBox
labelBox txt = do
  box <- hBoxNew False 0
  set box [containerBorderWidth := 2]
  label <- labelNew (Just txt)
  boxPackStart box label PackNatural 3
  return box

playAudio :: [Float] -> IO ()
playAudio audio = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  s <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  simpleWrite s audio
  simpleDrain s
  simpleFree s

playChord :: IO ()
playChord = do
  let times = [0..48000]
  let audio = map cMajor times
  playAudio audio

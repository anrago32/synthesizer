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
  button `onClicked` playChords
  box <- labelBox "Play Chords"
  button `containerAdd` box
  window `containerAdd` button
  widgetShowAll window
  window `onDestroy` mainQuit
  mainGUI

labelBox :: String -> IO HBox
labelBox text = do
  box <- hBoxNew False 0
  set box [containerBorderWidth := 2]
  label <- labelNew (Just text)
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

playChords :: IO ()
playChords = do
  let times = [0..24000]
  let audio1 = map cMajor times
  let audio2 = map fMajor times
  let audio = audio1 ++ audio2 ++ audio1
  playAudio audio

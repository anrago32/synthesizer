module Main where

import Graphics.UI.Gtk
import Sound.Pulse.Simple
import PitchConversion
import ToneGeneration

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "Program"
    , containerBorderWidth := 20
    ]

  button <- buttonNew
  onClicked button playChord
  box <- labelBox "Play Chord"
  containerAdd button box
  containerAdd window button

  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

labelBox :: String -> IO HBox
labelBox txt = do
  box   <- hBoxNew False 0
  set box [containerBorderWidth := 2]
  label <- labelNew (Just txt)
  boxPackStart box label PackNatural 3
  return box

playChord = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  s <- simpleNew Nothing "nothing"
    Play Nothing "nothing"
    sampleSpec Nothing Nothing
  let times = [0..24000]
  let tone1 = generateTone (C, 5) times
  let tone2 = generateTone (E, 5) times
  let tone3 = generateTone (G, 5) times
  let audio = generateChord [tone1, tone2, tone3]
  simpleWrite s audio
  simpleDrain s
  simpleFree s

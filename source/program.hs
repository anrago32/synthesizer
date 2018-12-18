module Main where

import Data.Text (pack)
import Graphics.UI.Gtk
import Sound.Pulse.Simple
import AudioGeneration
import PitchConversion

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "Synthesizer"
    , windowResizable := False
    ]

  widgetSetSizeRequest window 600 400
  table <- tableNew 8 64 False
  containerAdd window table

  -- First Row
  row1 <- hBoxNew True 10
  containerSetBorderWidth row1 10
  tableAttachDefaults table row1 0 8 0 1

  button1 <- buttonNew
  containerAdd row1 button1
  button2 <- buttonNew
  containerAdd row1 button2
  button3 <- buttonNew
  containerAdd row1 button3
  button4 <- buttonNew
  containerAdd row1 button4

  -- Second Row
  row2 <- hBoxNew True 10
  containerSetBorderWidth row2 10
  tableAttachDefaults table row2 0 8 1 32

  row2Section1 <- hBoxNew True 0
  containerAdd row2 row2Section1
  row2Section2 <- hBoxNew True 0
  containerAdd row2 row2Section2
  row2Section3 <- hBoxNew True 0
  containerAdd row2 row2Section3
  row2Section4 <- hBoxNew True 0
  containerAdd row2 row2Section4

  volumeSlider <- createSlider
  containerAdd row2Section1 volumeSlider
  pitchSlider <- createSlider
  containerAdd row2Section1 pitchSlider
  portamentoSlider <- createSlider
  containerAdd row2Section2 portamentoSlider
  envSlider1 <- createSlider
  containerAdd row2Section2 envSlider1
  envSlider2 <- createSlider
  containerAdd row2Section3 envSlider2
  envSlider3 <- createSlider
  containerAdd row2Section3 envSlider3
  envSlider4 <- createSlider
  containerAdd row2Section4 envSlider4
  envSlider5 <- createSlider
  containerAdd row2Section4 envSlider5

  -- Third Row
  row3 <- hBoxNew True 10
  containerSetBorderWidth row3 10
  tableAttachDefaults table row3 0 8 32 33

  oscSelector <- comboBoxNewText
  comboBoxAppendText oscSelector (pack "Frequency Mod")
  containerAdd row3 oscSelector
  lfoSelector <- comboBoxNewText
  comboBoxAppendText lfoSelector (pack "None")
  containerAdd row3 lfoSelector
  effSelector <- comboBoxNewText
  comboBoxAppendText effSelector (pack "None")
  containerAdd row3 effSelector
  filSelector <- comboBoxNewText
  comboBoxAppendText filSelector (pack "None")
  containerAdd row3 filSelector

  -- Fourth Row
  row4 <- hBoxNew True 10
  containerSetBorderWidth row4 10
  tableAttachDefaults table row4 0 8 33 64

  row4Section1 <- hBoxNew True 0
  containerAdd row4 row4Section1
  row4Section2 <- hBoxNew True 0
  containerAdd row4 row4Section2
  row4Section3 <- hBoxNew True 0
  containerAdd row4 row4Section3
  row4Section4 <- hBoxNew True 0
  containerAdd row4 row4Section4

  oscSlider1 <- createSlider
  containerAdd row4Section1 oscSlider1
  oscSlider2 <- createSlider
  containerAdd row4Section1 oscSlider2
  lfoSlider1 <- createSlider
  containerAdd row4Section2 lfoSlider1
  lfoSlider2 <- createSlider
  containerAdd row4Section2 lfoSlider2
  effSlider1 <- createSlider
  containerAdd row4Section3 effSlider1
  effSlider2 <- createSlider
  containerAdd row4Section3 effSlider2
  filSlider1 <- createSlider
  containerAdd row4Section4 filSlider1
  filSlider2 <- createSlider
  containerAdd row4Section4 filSlider2

  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

createSlider :: IO VScale
createSlider = do
  slider <- vScaleNewWithRange 0 99 1
  rangeSetInverted slider True
  scaleSetDrawValue slider False
  return slider

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

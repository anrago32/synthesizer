module Main where

import Data.Text (pack)
import Graphics.UI.Gtk
import Sound.Pulse.Simple
import AudioGeneration
import PitchConversion

main :: IO ()
main = do
  -- Window Initialization
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "Synthesizer"
    , windowResizable := False
    ]

  widgetSetSizeRequest window 600 400
  table <- tableNew 4 4 False
  containerAdd window table

  -- First Row
  row1 <- hBoxNew True 10
  containerSetBorderWidth row1 10
  tableAttachDefaults table row1 0 4 0 1
  widgetSetSizeRequest row1 600 0

  row1Section1 <- hBoxNew True 0
  row1Section2 <- hBoxNew True 0
  row1Section3 <- hBoxNew True 0
  row1Section4 <- hBoxNew True 0

  containerAdd row1 row1Section1
  containerAdd row1 row1Section2
  containerAdd row1 row1Section3
  containerAdd row1 row1Section4

  inputSelector <- comboBoxNewText
  scriptButton <- buttonNew
  loadButton <- buttonNew
  saveButton <- buttonNew

  containerAdd row1Section1 inputSelector
  containerAdd row1Section2 scriptButton
  containerAdd row1Section3 loadButton
  containerAdd row1Section4 saveButton

  comboBoxAppendText inputSelector $ pack "Keyboard"
  comboBoxAppendText inputSelector $ pack "MIDI"
  comboBoxSetActive inputSelector 0
  buttonSetLabel scriptButton "Process Script"
  buttonSetLabel loadButton "Load Patch"
  buttonSetLabel saveButton "Save Patch"

  -- Second Row
  row2 <- hBoxNew True 10
  containerSetBorderWidth row2 10
  tableAttachDefaults table row2 0 4 1 2
  widgetSetSizeRequest row2 600 100

  row2Section1 <- hBoxNew True 0
  row2Section2 <- hBoxNew True 0
  row2Section3 <- hBoxNew True 0
  row2Section4 <- hBoxNew True 0

  containerAdd row2 row2Section1
  containerAdd row2 row2Section2
  containerAdd row2 row2Section3
  containerAdd row2 row2Section4

  volumeSlider <- createSlider
  pitchSlider <- createSlider
  portamentoSlider <- createSlider
  envSlider1 <- createSlider
  envSlider2 <- createSlider
  envSlider3 <- createSlider
  envSlider4 <- createSlider
  envSlider5 <- createSlider

  containerAdd row2Section1 volumeSlider
  containerAdd row2Section1 pitchSlider
  containerAdd row2Section2 portamentoSlider
  containerAdd row2Section2 envSlider1
  containerAdd row2Section3 envSlider2
  containerAdd row2Section3 envSlider3
  containerAdd row2Section4 envSlider4
  containerAdd row2Section4 envSlider5

  -- Third Row
  row3 <- hBoxNew True 10
  containerSetBorderWidth row3 10
  tableAttachDefaults table row3 0 4 2 3
  widgetSetSizeRequest row3 600 0

  row3Section1 <- hBoxNew True 0
  row3Section2 <- hBoxNew True 0
  row3Section3 <- hBoxNew True 0
  row3Section4 <- hBoxNew True 0

  containerAdd row3 row3Section1
  containerAdd row3 row3Section2
  containerAdd row3 row3Section3
  containerAdd row3 row3Section4

  oscSelector <- comboBoxNewText
  filSelector <- comboBoxNewText
  lfoSelector <- comboBoxNewText
  effSelector <- comboBoxNewText

  containerAdd row3Section1 oscSelector
  containerAdd row3Section4 filSelector
  containerAdd row3Section2 lfoSelector
  containerAdd row3Section3 effSelector

  comboBoxAppendText oscSelector $ pack "Frequency Mod"
  comboBoxSetActive oscSelector 0
  comboBoxAppendText filSelector $ pack "None"
  comboBoxSetActive filSelector 0
  comboBoxAppendText lfoSelector $ pack "None"
  comboBoxSetActive lfoSelector 0
  comboBoxAppendText effSelector $ pack "None"
  comboBoxSetActive effSelector 0

  -- Fourth Row
  row4 <- hBoxNew True 10
  containerSetBorderWidth row4 10
  tableAttachDefaults table row4 0 4 3 4
  widgetSetSizeRequest row4 600 100

  row4Section1 <- hBoxNew True 0
  row4Section2 <- hBoxNew True 0
  row4Section3 <- hBoxNew True 0
  row4Section4 <- hBoxNew True 0

  containerAdd row4 row4Section1
  containerAdd row4 row4Section2
  containerAdd row4 row4Section3
  containerAdd row4 row4Section4

  oscSlider1 <- createSlider
  oscSlider2 <- createSlider
  filSlider1 <- createSlider
  filSlider2 <- createSlider
  lfoSlider1 <- createSlider
  lfoSlider2 <- createSlider
  effSlider1 <- createSlider
  effSlider2 <- createSlider

  containerAdd row4Section1 oscSlider1
  containerAdd row4Section1 oscSlider2
  containerAdd row4Section4 filSlider1
  containerAdd row4Section4 filSlider2
  containerAdd row4Section2 lfoSlider1
  containerAdd row4Section2 lfoSlider2
  containerAdd row4Section3 effSlider1
  containerAdd row4Section3 effSlider2

  -- Window Initiation
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

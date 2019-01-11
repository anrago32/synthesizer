module Main where

import Data.Text (pack)
import Graphics.UI.Gtk
import Sound.Pulse.Simple
import AudioGeneration
import Patch
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

  table <- tableNew 4 4 False
  containerAdd window table
  onDestroy window mainQuit
  widgetModifyBg window StateNormal (Color maxBound maxBound maxBound)
  widgetSetSizeRequest window 600 400

  -- First Row
  row1 <- hBoxNew True 10
  containerSetBorderWidth row1 10
  tableAttachDefaults table row1 0 4 0 1
  widgetSetSizeRequest row1 0 0

  row1Section1 <- hBoxNew True 0
  row1Section2 <- hBoxNew True 0
  row1Section3 <- hBoxNew True 0
  row1Section4 <- hBoxNew True 0
  containerAdd row1 row1Section1
  containerAdd row1 row1Section2
  containerAdd row1 row1Section3
  containerAdd row1 row1Section4

  inputSelector <- comboBoxNewText
  scriptButton  <- buttonNew
  loadButton    <- buttonNew
  saveButton    <- buttonNew
  containerAdd row1Section1 inputSelector
  containerAdd row1Section2 scriptButton
  containerAdd row1Section3 loadButton
  containerAdd row1Section4 saveButton

  comboBoxAppendText inputSelector $ pack "Keyboard"
  comboBoxAppendText inputSelector $ pack "MIDI"
  buttonSetLabel scriptButton "Process Script"
  buttonSetLabel loadButton "Load Patch"
  buttonSetLabel saveButton "Save Patch"

  -- Second Row
  row2 <- hBoxNew True 0
  tableAttachDefaults table row2 0 4 1 2
  widgetSetSizeRequest row2 0 100

  row2Section1 <- hBoxNew True 0
  row2Section2 <- hBoxNew True 0
  row2Section3 <- hBoxNew True 0
  row2Section4 <- hBoxNew True 0
  containerAdd row2 row2Section1
  containerAdd row2 row2Section2
  containerAdd row2 row2Section3
  containerAdd row2 row2Section4

  volumeSlider <- createSlider "Vol"
  octaveSlider <- createSlider "Oct"
  glissSlider  <- createSlider "Gls"
  envSlider1   <- createSlider "Env"
  envSlider2   <- createSlider "A"
  envSlider3   <- createSlider "D"
  envSlider4   <- createSlider "S"
  envSlider5   <- createSlider "R"

  containerAdd row2Section1 volumeSlider
  containerAdd row2Section1 octaveSlider
  containerAdd row2Section2 glissSlider
  containerAdd row2Section2 envSlider1
  containerAdd row2Section3 envSlider2
  containerAdd row2Section3 envSlider3
  containerAdd row2Section4 envSlider4
  containerAdd row2Section4 envSlider5

  -- Third Row
  row3 <- hBoxNew True 10
  containerSetBorderWidth row3 10
  tableAttachDefaults table row3 0 4 2 3
  widgetSetSizeRequest row3 0 0

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
  containerAdd row3Section2 filSelector
  containerAdd row3Section3 lfoSelector
  containerAdd row3Section4 effSelector

  comboBoxAppendText oscSelector $ pack "AM Oscillator"
  comboBoxAppendText oscSelector $ pack "FM Oscillator"
  comboBoxAppendText oscSelector $ pack "Pulse Oscillator"
  comboBoxAppendText oscSelector $ pack "Saw Oscillator"
  comboBoxAppendText oscSelector $ pack "Sync Oscillator"
  comboBoxAppendText filSelector $ pack "No Filter"
  comboBoxAppendText filSelector $ pack "Band-Pass Filter"
  comboBoxAppendText filSelector $ pack "High-Pass Filter"
  comboBoxAppendText filSelector $ pack "Low-Pass Filter"
  comboBoxAppendText lfoSelector $ pack "No Lfo"
  comboBoxAppendText lfoSelector $ pack "Amplitude Lfo"
  comboBoxAppendText lfoSelector $ pack "Filter Lfo"
  comboBoxAppendText lfoSelector $ pack "Frequency Lfo"
  comboBoxAppendText effSelector $ pack "No Effect"
  comboBoxAppendText effSelector $ pack "Chorus Effect"
  comboBoxAppendText effSelector $ pack "Delay Effect"
  comboBoxAppendText effSelector $ pack "Distortion Effect"
  comboBoxAppendText effSelector $ pack "Phaser Effect"

  -- Fourth Row
  row4 <- hBoxNew True 0
  tableAttachDefaults table row4 0 4 3 4
  widgetSetSizeRequest row4 0 100

  row4Section1 <- hBoxNew True 0
  row4Section2 <- hBoxNew True 0
  row4Section3 <- hBoxNew True 0
  row4Section4 <- hBoxNew True 0
  containerAdd row4 row4Section1
  containerAdd row4 row4Section2
  containerAdd row4 row4Section3
  containerAdd row4 row4Section4

  oscSlider1 <- createSlider "Mod"
  oscSlider2 <- createSlider "Tex"
  filSlider1 <- createSlider "Cut"
  filSlider2 <- createSlider "Res"
  lfoSlider1 <- createSlider "Depth"
  lfoSlider2 <- createSlider "Rate"
  effSlider1 <- createSlider "Depth"
  effSlider2 <- createSlider "Rate"

  containerAdd row4Section1 oscSlider1
  containerAdd row4Section1 oscSlider2
  containerAdd row4Section2 filSlider1
  containerAdd row4Section2 filSlider2
  containerAdd row4Section3 lfoSlider1
  containerAdd row4Section3 lfoSlider2
  containerAdd row4Section4 effSlider1
  containerAdd row4Section4 effSlider2

  -- Window Initiation
  widgetShowAll window
  mainGUI

createSlider :: String -> IO VBox
createSlider text = do
  slider <- vBoxNew False 0
  scale  <- vScaleNewWithRange 0 100 1
  label  <- labelNew $ Just text
  containerAdd slider scale
  containerAdd slider label
  rangeSetInverted scale True
  scaleSetDrawValue scale False
  widgetSetSizeRequest scale 0 100
  widgetSetSizeRequest label 0 0
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

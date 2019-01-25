module Gui where

import Data.Text (pack)
import Graphics.UI.Gtk

data Gui = Gui
  { loadButton       :: Button
  , saveButton       :: Button
  , scriptButton     :: Button
  , effectCombo      :: ComboBox
  , filterCombo      :: ComboBox
  , inputCombo       :: ComboBox
  , lfoCombo         :: ComboBox
  , oscillatorCombo  :: ComboBox
  , attackScale      :: VScale
  , cutoffScale      :: VScale
  , decayScale       :: VScale
  , effectDepthScale :: VScale
  , effectRateScale  :: VScale
  , envelopeScale    :: VScale
  , glissandoScale   :: VScale
  , lfoDepthScale    :: VScale
  , lfoRateScale     :: VScale
  , modulationScale  :: VScale
  , octaveScale      :: VScale
  , releaseScale     :: VScale
  , resonanceScale   :: VScale
  , sustainScale     :: VScale
  , textureScale     :: VScale
  , volumeScale      :: VScale
  }

createGui :: Window -> IO Gui
createGui window = do
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

  combo1  <- comboBoxNewText
  button1 <- buttonNew
  button2 <- buttonNew
  button3 <- buttonNew
  containerAdd row1Section1 combo1
  containerAdd row1Section2 button1
  containerAdd row1Section3 button2
  containerAdd row1Section4 button3

  comboBoxAppendText combo1 $ pack "Keyboard Input"
  comboBoxAppendText combo1 $ pack "MIDI Input"
  buttonSetLabel button1 "Process Script"
  buttonSetLabel button2 "Load Patch"
  buttonSetLabel button3 "Save Patch"

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

  scale1 <- vScaleNewWithRange 0 100 1
  scale2 <- vScaleNewWithRange 0 100 1
  scale3 <- vScaleNewWithRange 0 100 1
  scale4 <- vScaleNewWithRange 0 100 1
  scale5 <- vScaleNewWithRange 0 100 1
  scale6 <- vScaleNewWithRange 0 100 1
  scale7 <- vScaleNewWithRange 0 100 1
  scale8 <- vScaleNewWithRange 0 100 1

  slider1 <- createSlider scale1 "Vol"
  slider2 <- createSlider scale2 "Oct"
  slider3 <- createSlider scale3 "Gls"
  slider4 <- createSlider scale4 "Env"
  slider5 <- createSlider scale5 "A"
  slider6 <- createSlider scale6 "D"
  slider7 <- createSlider scale7 "S"
  slider8 <- createSlider scale8 "R"

  containerAdd row2Section1 slider1
  containerAdd row2Section1 slider2
  containerAdd row2Section2 slider3
  containerAdd row2Section2 slider4
  containerAdd row2Section3 slider5
  containerAdd row2Section3 slider6
  containerAdd row2Section4 slider7
  containerAdd row2Section4 slider8

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

  combo2 <- comboBoxNewText
  combo3 <- comboBoxNewText
  combo4 <- comboBoxNewText
  combo5 <- comboBoxNewText
  containerAdd row3Section1 combo2
  containerAdd row3Section2 combo3
  containerAdd row3Section3 combo4
  containerAdd row3Section4 combo5

  comboBoxAppendText combo2 $ pack "AM Oscillator"
  comboBoxAppendText combo2 $ pack "FM Oscillator"
  comboBoxAppendText combo2 $ pack "Pulse Oscillator"
  comboBoxAppendText combo2 $ pack "Saw Oscillator"
  comboBoxAppendText combo2 $ pack "Sync Oscillator"
  comboBoxAppendText combo3 $ pack "No Filter"
  comboBoxAppendText combo3 $ pack "Band-Pass Filter"
  comboBoxAppendText combo3 $ pack "High-Pass Filter"
  comboBoxAppendText combo3 $ pack "Low-Pass Filter"
  comboBoxAppendText combo4 $ pack "No Lfo"
  comboBoxAppendText combo4 $ pack "Amplitude Lfo"
  comboBoxAppendText combo4 $ pack "Filter Lfo"
  comboBoxAppendText combo4 $ pack "Frequency Lfo"
  comboBoxAppendText combo5 $ pack "No Effect"
  comboBoxAppendText combo5 $ pack "Chorus Effect"
  comboBoxAppendText combo5 $ pack "Delay Effect"
  comboBoxAppendText combo5 $ pack "Distortion Effect"
  comboBoxAppendText combo5 $ pack "Phaser Effect"

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

  scale9  <- vScaleNewWithRange 0 100 1
  scale10 <- vScaleNewWithRange 0 100 1
  scale11 <- vScaleNewWithRange 0 100 1
  scale12 <- vScaleNewWithRange 0 100 1
  scale13 <- vScaleNewWithRange 0 100 1
  scale14 <- vScaleNewWithRange 0 100 1
  scale15 <- vScaleNewWithRange 0 100 1
  scale16 <- vScaleNewWithRange 0 100 1

  slider9  <- createSlider scale9  "Mod"
  slider10 <- createSlider scale10 "Tex"
  slider11 <- createSlider scale11 "Cut"
  slider12 <- createSlider scale12 "Res"
  slider13 <- createSlider scale13 "Depth"
  slider14 <- createSlider scale14 "Rate"
  slider15 <- createSlider scale15 "Depth"
  slider16 <- createSlider scale16 "Rate"

  containerAdd row4Section1 slider9
  containerAdd row4Section1 slider10
  containerAdd row4Section2 slider11
  containerAdd row4Section2 slider12
  containerAdd row4Section3 slider13
  containerAdd row4Section3 slider14
  containerAdd row4Section4 slider15
  containerAdd row4Section4 slider16

  return Gui
    { loadButton       = button2
    , saveButton       = button3
    , scriptButton     = button1
    , effectCombo      = combo5
    , filterCombo      = combo3
    , inputCombo       = combo1
    , lfoCombo         = combo4
    , oscillatorCombo  = combo2
    , attackScale      = scale1
    , cutoffScale      = scale2
    , decayScale       = scale3
    , effectDepthScale = scale4
    , effectRateScale  = scale5
    , envelopeScale    = scale6
    , glissandoScale   = scale7
    , lfoDepthScale    = scale8
    , lfoRateScale     = scale9
    , modulationScale  = scale10
    , octaveScale      = scale11
    , releaseScale     = scale12
    , resonanceScale   = scale13
    , sustainScale     = scale14
    , textureScale     = scale15
    , volumeScale      = scale16
    }

createSlider :: VScale -> String -> IO VBox
createSlider scale text = do
  slider <- vBoxNew False 0
  label  <- labelNew $ Just text
  containerAdd slider scale
  containerAdd slider label
  rangeSetInverted scale True
  scaleSetDrawValue scale False
  widgetSetSizeRequest scale 0 100
  widgetSetSizeRequest label 0 0
  return slider

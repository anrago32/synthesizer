module Gui where

import Data.Text (pack)
import Graphics.UI.Gtk

data ElementSize = Large | Small
  deriving (Eq)

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

createGui :: IO Gui
createGui = do
  -- Window Initialization
  mainWindow <- createWindow "Synthesizer"
  onDestroy mainWindow mainQuit
  table <- tableNew 4 4 False
  containerAdd mainWindow table

  -- First Row
  row1 <- createRow Small
  tableAttachDefaults table row1 0 4 0 1
  row1Section1 <- newSection row1
  row1Section2 <- newSection row1
  row1Section3 <- newSection row1
  row1Section4 <- newSection row1

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
  row2 <- createRow Large
  tableAttachDefaults table row2 0 4 1 2
  row2Section1 <- newSection row2
  row2Section2 <- newSection row2
  row2Section3 <- newSection row2
  row2Section4 <- newSection row2

  scale1 <- vScaleNewWithRange 0 100 1
  scale2 <- vScaleNewWithRange 0 100 1
  scale3 <- vScaleNewWithRange 0 100 1
  scale4 <- vScaleNewWithRange 0 100 1
  scale5 <- vScaleNewWithRange 0 100 1
  scale6 <- vScaleNewWithRange 0 100 1
  scale7 <- vScaleNewWithRange 0 100 1
  scale8 <- vScaleNewWithRange 0 100 1

  addLabelScale row2Section1 scale1 "Vol"
  addLabelScale row2Section1 scale2 "Oct"
  addLabelScale row2Section2 scale3 "Gls"
  addLabelScale row2Section2 scale4 "Env"
  addLabelScale row2Section3 scale5 "A"
  addLabelScale row2Section3 scale6 "D"
  addLabelScale row2Section4 scale7 "S"
  addLabelScale row2Section4 scale8 "R"

  -- Third Row
  row3 <- createRow Small
  tableAttachDefaults table row3 0 4 2 3
  row3Section1 <- newSection row3
  row3Section2 <- newSection row3
  row3Section3 <- newSection row3
  row3Section4 <- newSection row3

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
  row4 <- createRow Large
  tableAttachDefaults table row4 0 4 3 4
  row4Section1 <- newSection row4
  row4Section2 <- newSection row4
  row4Section3 <- newSection row4
  row4Section4 <- newSection row4

  scale9  <- vScaleNewWithRange 0 100 1
  scale10 <- vScaleNewWithRange 0 100 1
  scale11 <- vScaleNewWithRange 0 100 1
  scale12 <- vScaleNewWithRange 0 100 1
  scale13 <- vScaleNewWithRange 0 100 1
  scale14 <- vScaleNewWithRange 0 100 1
  scale15 <- vScaleNewWithRange 0 100 1
  scale16 <- vScaleNewWithRange 0 100 1

  addLabelScale row4Section1 scale9  "Mod"
  addLabelScale row4Section1 scale10 "Tex"
  addLabelScale row4Section2 scale11 "Cut"
  addLabelScale row4Section2 scale12 "Res"
  addLabelScale row4Section3 scale13 "Depth"
  addLabelScale row4Section3 scale14 "Rate"
  addLabelScale row4Section4 scale15 "Depth"
  addLabelScale row4Section4 scale16 "Rate"

  -- Window Initiation
  widgetShowAll mainWindow

  return Gui
    { loadButton       = button2
    , saveButton       = button3
    , scriptButton     = button1
    , effectCombo      = combo5
    , filterCombo      = combo3
    , inputCombo       = combo1
    , lfoCombo         = combo4
    , oscillatorCombo  = combo2
    , attackScale      = scale5
    , cutoffScale      = scale11
    , decayScale       = scale6
    , effectDepthScale = scale15
    , effectRateScale  = scale16
    , envelopeScale    = scale4
    , glissandoScale   = scale3
    , lfoDepthScale    = scale13
    , lfoRateScale     = scale14
    , modulationScale  = scale9
    , octaveScale      = scale2
    , releaseScale     = scale8
    , resonanceScale   = scale12
    , sustainScale     = scale7
    , textureScale     = scale10
    , volumeScale      = scale1
    }

addLabelScale :: HBox -> VScale -> String -> IO ()
addLabelScale section scale text = do
  slider <- vBoxNew False 0
  label  <- labelNew $ Just text
  containerAdd slider scale
  containerAdd slider label
  rangeSetInverted scale True
  scaleSetDrawValue scale False
  widgetSetSizeRequest scale 0 100
  widgetSetSizeRequest label 0 0
  containerAdd section slider

createRow :: ElementSize -> IO HBox
createRow Large = do
  row <- hBoxNew True 10
  widgetSetSizeRequest row 0 100
  return row
createRow Small = do
  row <- hBoxNew True 10
  containerSetBorderWidth row 10
  widgetSetSizeRequest row 0 0
  return row

createWindow :: String -> IO Window
createWindow title = do
  window <- windowNew
  set window
    [ windowTitle := "Synthesizer"
    , windowResizable := False
    ]
  widgetModifyBg window StateNormal $ Color maxBound maxBound maxBound
  widgetSetSizeRequest window 600 400
  return window

newSection :: HBox -> IO HBox
newSection row = do
  section <- hBoxNew True 0
  containerAdd row section
  return section

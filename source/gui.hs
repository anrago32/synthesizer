-- GUI
-- Description goes here
-- Written by Alex Rago, 2020

module GUI where

import Data.Text (pack)
import Graphics.UI.Gtk
import Patch

data GUI = GUI
  { adjustButton     :: Button
  , loadButton       :: Button
  , saveButton       :: Button
  , inputCombo       :: ComboBox
  , oscillatorCombo  :: ComboBox
  , filterCombo      :: ComboBox
  , lfoCombo         :: ComboBox
  , effectCombo      :: ComboBox
  , volumeScale      :: VScale
  , octaveScale      :: VScale
  , glissandoScale   :: VScale
  , envelopeScale    :: VScale
  , attackScale      :: VScale
  , decayScale       :: VScale
  , sustainScale     :: VScale
  , releaseScale     :: VScale
  , modulationScale  :: VScale
  , textureScale     :: VScale
  , cutoffScale      :: VScale
  , resonanceScale   :: VScale
  , lfoDepthScale    :: VScale
  , lfoRateScale     :: VScale
  , effectDepthScale :: VScale
  , effectRateScale  :: VScale
  , mainWindow       :: Window
  }

createGUI :: IO GUI
createGUI = do
  -- Window Creation
  mainWindow <- createWindow "Synthesizer" 600 400
  table <- tableNew 4 4 False
  containerAdd mainWindow table
  onDestroy mainWindow mainQuit

  -- First Row
  row1 <- createRow SmallRow
  tableAttachDefaults table row1 0 4 0 1
  row1Section1 <- newSection row1
  row1Section2 <- newSection row1
  row1Section3 <- newSection row1
  row1Section4 <- newSection row1

  combo1 <- newCombo row1Section1
  button1 <- newButton row1Section2
  button2 <- newButton row1Section3
  button3 <- newButton row1Section4

  addComboEntry combo1 "Keyboard Input"
  addComboEntry combo1 "MIDI Input"
  buttonSetLabel button1 "Adjust Settings"
  buttonSetLabel button2 "Load Patch"
  buttonSetLabel button3 "Save Patch"
  comboBoxSetActive combo1 0

  -- Second Row
  row2 <- createRow LargeRow
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
  addLabelScale row2Section1 scale4 "Env"
  addLabelScale row2Section4 scale5 "A"
  addLabelScale row2Section4 scale6 "D"
  addLabelScale row2Section4 scale7 "S"
  addLabelScale row2Section4 scale8 "R"

  -- Third Row
  row3 <- createRow SmallRow
  tableAttachDefaults table row3 0 4 2 3
  row3Section1 <- newSection row3
  row3Section2 <- newSection row3
  row3Section3 <- newSection row3
  row3Section4 <- newSection row3

  combo2 <- newCombo row3Section1
  combo3 <- newCombo row3Section2
  combo4 <- newCombo row3Section3
  combo5 <- newCombo row3Section4

  addComboEntry combo2 "Am Osc"
  addComboEntry combo2 "Fm Osc"
  addComboEntry combo2 "Pulse Osc"
  addComboEntry combo2 "Saw Osc"
  addComboEntry combo2 "Sync Osc"
  addComboEntry combo3 "No Filter"
  addComboEntry combo3 "Highpass Filter"
  addComboEntry combo3 "Lowpass Filter"
  addComboEntry combo4 "No Lfo"
  addComboEntry combo4 "Amplitude Lfo"
  addComboEntry combo4 "Cutoff Lfo"
  addComboEntry combo4 "Frequency Lfo"
  addComboEntry combo4 "Modulation Lfo"
  addComboEntry combo5 "No Effect"
  addComboEntry combo5 "Chorus Effect"
  addComboEntry combo5 "Delay Effect"

  -- Fourth Row
  row4 <- createRow LargeRow
  tableAttachDefaults table row4 0 4 3 4
  row4Section1 <- newSection row4
  row4Section2 <- newSection row4
  row4Section3 <- newSection row4
  row4Section4 <- newSection row4

  scale9 <- vScaleNewWithRange 0 100 1
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

  widgetShowAll mainWindow
  return $ GUI button1 button2 button3 combo1 combo2 combo3 combo4 combo5 scale1
    scale2 scale3 scale4 scale5 scale6 scale7 scale8 scale9 scale10 scale11
    scale12 scale13 scale14 scale15 scale16 mainWindow

adjustSettings :: GUI -> IO ()
adjustSettings gui = do
  configurationWindow <- createWindow "Adjust Settings" 300 200
  widgetShowAll configurationWindow

loadPatch :: GUI -> IO ()
loadPatch gui = do
  loadWindow <- createWindow "Load Patch" 150 100
  widgetShowAll loadWindow
  --table <- tableNew 4 4 False
  --topRow <- createRow SmallRow
  --topSection <- newSection topRow
  --combo <- newCombo topSection
  --containerAdd topRow table
  --containerAdd loadWindow table

savePatch :: GUI -> IO ()
savePatch gui = do
  saveWindow <- createWindow "Save Patch" 150 100
  widgetShowAll saveWindow
  --table <- tableNew 4 4 False
  --topRow <- createRow SmallRow
  --topSection <- newSection topRow
  --combo <- newCombo topSection
  --containerAdd topRow table
  --containerAdd saveWindow table

getPatch :: GUI -> IO Patch
getPatch gui = do
  a <- comboBoxGetActive $ oscillatorCombo  gui
  b <- comboBoxGetActive $ filterCombo      gui
  c <- comboBoxGetActive $ lfoCombo         gui
  d <- comboBoxGetActive $ effectCombo      gui
  e <- rangeGetValue     $ volumeScale      gui
  f <- rangeGetValue     $ octaveScale      gui
  g <- rangeGetValue     $ glissandoScale   gui
  h <- rangeGetValue     $ envelopeScale    gui
  i <- rangeGetValue     $ attackScale      gui
  j <- rangeGetValue     $ decayScale       gui
  k <- rangeGetValue     $ sustainScale     gui
  l <- rangeGetValue     $ releaseScale     gui
  m <- rangeGetValue     $ modulationScale  gui
  n <- rangeGetValue     $ textureScale     gui
  o <- rangeGetValue     $ cutoffScale      gui
  p <- rangeGetValue     $ resonanceScale   gui
  q <- rangeGetValue     $ lfoDepthScale    gui
  r <- rangeGetValue     $ lfoRateScale     gui
  s <- rangeGetValue     $ effectDepthScale gui
  t <- rangeGetValue     $ effectRateScale  gui
  return $ Patch a b c d e f g h i j k l m n o p q r s t

setPatch :: GUI -> String -> IO ()
setPatch gui file = do
  patch <- read <$> readFile file
  comboBoxSetActive (oscillatorCombo  gui) (oscillatorType patch)
  comboBoxSetActive (filterCombo      gui) (filterType     patch)
  comboBoxSetActive (lfoCombo         gui) (lfoType        patch)
  comboBoxSetActive (effectCombo      gui) (effectType     patch)
  rangeSetValue     (volumeScale      gui) (volume         patch)
  rangeSetValue     (octaveScale      gui) (octave         patch)
  rangeSetValue     (glissandoScale   gui) (glissando      patch)
  rangeSetValue     (envelopeScale    gui) (envelope       patch)
  rangeSetValue     (attackScale      gui) (attack         patch)
  rangeSetValue     (decayScale       gui) (decay          patch)
  rangeSetValue     (sustainScale     gui) (sustain        patch)
  rangeSetValue     (releaseScale     gui) (release        patch)
  rangeSetValue     (modulationScale  gui) (modulation     patch)
  rangeSetValue     (textureScale     gui) (texture        patch)
  rangeSetValue     (cutoffScale      gui) (cutoff         patch)
  rangeSetValue     (resonanceScale   gui) (resonance      patch)
  rangeSetValue     (lfoDepthScale    gui) (lfoDepth       patch)
  rangeSetValue     (lfoRateScale     gui) (lfoRate        patch)
  rangeSetValue     (effectDepthScale gui) (effectDepth    patch)
  rangeSetValue     (effectRateScale  gui) (effectRate     patch)

data RowSize = LargeRow | SmallRow

addComboEntry :: ComboBox -> String -> IO Int
addComboEntry combo text = comboBoxAppendText combo $ pack text

addLabelScale :: HBox -> VScale -> String -> IO ()
addLabelScale section scale text = do
  slider <- vBoxNew False 0
  label <- labelNew $ Just text
  containerAdd slider scale
  containerAdd slider label
  rangeSetInverted scale True
  scaleSetDrawValue scale False
  widgetSetSizeRequest scale 0 100
  widgetSetSizeRequest label 0 0
  containerAdd section slider

createRow :: RowSize -> IO HBox
createRow LargeRow = do
  row <- hBoxNew True 10
  widgetSetSizeRequest row 0 100
  return row
createRow SmallRow = do
  row <- hBoxNew True 10
  containerSetBorderWidth row 10
  widgetSetSizeRequest row 0 0
  return row

createWindow :: String -> Int -> Int -> IO Window
createWindow title x y = do
  window <- windowNew
  set window
    [ windowTitle := title
    , windowResizable := False
    ]
  widgetModifyBg window StateNormal $ Color maxBound maxBound maxBound
  widgetSetSizeRequest window x y
  return window

newButton :: HBox -> IO Button
newButton section = do
  button <- buttonNew
  containerAdd section button
  return button

newCombo :: HBox -> IO ComboBox
newCombo section = do
  combo <- comboBoxNewText
  containerAdd section combo
  return combo

newSection :: HBox -> IO HBox
newSection row = do
  section <- hBoxNew True 0
  containerAdd row section
  return section

module Widgets where

import Data.Text (pack)
import Graphics.UI.Gtk

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

createWindow :: String -> IO Window
createWindow title = do
  window <- windowNew
  set window
    [ windowTitle := title
    , windowResizable := False
    ]
  widgetModifyBg window StateNormal $ Color maxBound maxBound maxBound
  widgetSetSizeRequest window 600 400
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

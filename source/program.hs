-- Synthesizer
-- Audio synthesizer written in Haskell
-- Written by Alex Rago, 2020

module Main where


import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Sound.Pulse.Simple
import Audio
import Gui
import Patch
import Pitch


main :: IO ()
main = do
  initGUI
  gui <- createGui
  setPatch gui "/home/anrago/Code/synthesizer/files/default.patch"
  onClicked (scriptButton gui) (processScript gui)
  onClicked (loadButton gui) (loadPatch gui)
  onClicked (saveButton gui) (savePatch gui)
  onKeyPress (mainWindow gui) handleKeyPress
  mainGUI


handleKeyPress :: Event -> IO Bool
handleKeyPress event = do
  case event of
    (Key _ _ _ _ _ _ _ _ _ (Just character)) ->
      case character of
        'z' -> play (C, 4)
        's' -> play (Cs, 4)
        'x' -> play (D, 4)
        'd' -> play (Ds, 4)
        'c' -> play (E, 4)
        'v' -> play (F, 4)
        'g' -> play (Fs, 4)
        'b' -> play (G, 4)
        'h' -> play (Gs, 4)
        'n' -> play (A, 4)
        'j' -> play (As, 4)
        'm' -> play (B, 4)
        ',' -> play (C, 5)
        'l' -> play (Cs, 5)
        '.' -> play (D, 5)
        ';' -> play (Ds, 5)
        '/' -> play (E, 5)
        'q' -> play (C, 6)
        '2' -> play (Cs, 6)
        'w' -> play (D, 6)
        '3' -> play (Ds, 6)
        'e' -> play (E, 6)
        'r' -> play (F, 6)
        '5' -> play (Fs, 6)
        't' -> play (G, 6)
        '6' -> play (Gs, 6)
        'y' -> play (A, 6)
        '7' -> play (As, 6)
        'u' -> play (B, 6)
        'i' -> play (C, 7)
        '9' -> play (Cs, 7)
        'o' -> play (D, 7)
        '0' -> play (Ds, 7)
        'p' -> play (E, 7)
        '[' -> play (F, 7)
        '=' -> play (Fs, 7)
        ']' -> play (G, 7)
        '\\' -> play (A, 7)
        _ -> return ()
    _ -> return ()
  return True


play :: Pitch -> IO ()
play pitch = do
  let sampleFormat = F32 LittleEndian
  let sampleSpec = SampleSpec sampleFormat 48000 1
  let times = [0..12000]

  let audio = fmOsc pitch <$> times
  s <- simpleNew Nothing "" Play Nothing "" sampleSpec Nothing Nothing
  simpleWrite s audio
  simpleFree s

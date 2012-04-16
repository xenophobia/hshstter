module GUILibrary where

import Data.Word
import Graphics.UI.Gtk hiding (add)
import Graphics.UI.Gtk.Gdk.GC
import Control.Concurrent

type RGB = (Word16, Word16, Word16)
type Coordinate = (Int, Int)

drawString :: DrawingArea -> DrawWindow -> RGB -> Coordinate -> Int -> String -> IO ()
drawString drawArea drawWin (r, g, b) (x, y) width str = do
  gc <- gcNewWithValues drawWin $ newGCValues {foreground = Color r g b}
  layout <- widgetCreateLayout drawArea str
  layoutSetText layout str
  layoutSetWidth layout . Just . fromIntegral $ width
  drawLayout drawWin gc x y layout
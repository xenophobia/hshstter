module GUILibrary where

import TweetJSON

import Data.Word
import Data.Maybe
import Control.Applicative
import Graphics.UI.Gtk hiding (add)
import Graphics.UI.Gtk.Gdk.GC
import Control.Concurrent

type RGB = (Word16, Word16, Word16)
type Coordinate = (Int, Int)
type Icon = Pixbuf

textViewGetText ::TextViewClass self => self -> Bool -> IO String
textViewGetText textview includeHiddenChars = do
  tb <- textViewGetBuffer textview
  stitr <- textBufferGetStartIter tb
  editr <- textBufferGetEndIter tb
  textBufferGetText tb stitr editr includeHiddenChars

drawString :: DrawingArea -> DrawWindow -> RGB -> Coordinate -> Int -> String -> IO ()
drawString drawArea drawWin (r, g, b) (x, y) width str = const () <$> do
  gc <- gcNewWithValues drawWin $ newGCValues {foreground = Color r g b}
  layout <- widgetCreateLayout drawArea str
  layoutSetText layout str
  layoutSetWidth layout . Just . fromIntegral $ width
  drawLayout drawWin gc x y layout

borderColor1 :: GCValues
borderColor1 = newGCValues {foreground = Color 16384 16384 16384}
borderColor2 :: GCValues
borderColor2 = newGCValues {foreground = Color 49152 49152 49152}

drawSeparateLine :: DrawWindow -> Coordinate -> Int -> IO ()
drawSeparateLine drawWin (x, y) lineLength = do
  gc <- gcNewWithValues drawWin borderColor1
  drawLine drawWin gc (x, y) (x + lineLength, y)
  gcSetValues gc borderColor2
  drawLine drawWin gc (x, y+1) (x + lineLength, y+1)

drawIcon :: DrawWindow -> Pixbuf -> Coordinate -> IO ()
drawIcon drawWin icon (x, y) = do
  gc <- gcNew drawWin
  drawPixbuf drawWin gc icon 0 0 (x + 6) (y + 10) 48 48 RgbDitherNone (-1) (-1)

tweetAreaHeight :: Int
tweetAreaHeight = 68
tweetAreaMargin :: Int
tweetAreaMargin = 5

drawTweet :: DrawingArea -> DrawWindow -> Pixbuf -> RGB -> Int -> Int -> Tweet -> IO Int
drawTweet drawingArea drawWin icon (r, g, b) width height twt = do
  drawString drawingArea drawWin (r, g, b) (tweetAreaHeight , (height + tweetAreaMargin)) (width - tweetAreaHeight) (show twt)
  drawIcon drawWin icon (0, height)
  drawSeparateLine drawWin (0, height + tweetAreaHeight) (width + 30)
  return $ height + tweetAreaHeight

scrolledWindowNewWithWidget :: (WidgetClass widget) => widget -> IO ScrolledWindow
scrolledWindowNewWithWidget widget = do
  sc <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport sc widget
  scrolledWindowSetPolicy sc PolicyAutomatic PolicyAutomatic
  return sc
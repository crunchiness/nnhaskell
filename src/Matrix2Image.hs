module Matrix2Image where

import Data.Array.MArray (writeArray)
import Data.Matrix
import Data.Word

import Graphics.UI.Gtk (eventWindow)
import Graphics.UI.Gtk.Abstract.Widget (onDestroy, widgetGetDrawWindow, widgetSetSizeRequest, widgetShowAll)
import Graphics.UI.Gtk.General.General (initGUI, mainGUI, mainQuit, timeoutAdd)
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.GC (gcNew)
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Windows.Window (windowNew)

createPixbuf :: Int -> Int -> IO Pixbuf
createPixbuf w h = pixbufNew ColorspaceRgb False 8 w h

setRGB :: Pixbuf -> Int -> Int -> (Word8, Word8, Word8) -> IO ()
setRGB pb x y (r, g, b) = do
    ps <- pixbufGetPixels pb :: IO (PixbufData Int Word8)
    row <- pixbufGetRowstride pb
    writeArray ps (y * row + 3 * x + 0) r
    writeArray ps (y * row + 3 * x + 1) g
    writeArray ps (y * row + 3 * x + 2) b

setPixels :: RealFrac a => Pixbuf -> Matrix a -> IO [()]
setPixels pb m = do
                     sequence [ f (x - 1) (y - 1) $ m ! (x, y) | y <- [1 .. (nrows m)], x <- [1 .. (ncols m)] ]
                     where
                         f x y val = do
                             let v = (round val :: Word8)
                             setRGB pb x y (v, v, v)


displayMatrix :: RealFrac a => Matrix a -> IO ()
displayMatrix m = do
    initGUI
    let w = ncols m; h = nrows m
    window <- windowNew
    onDestroy window mainQuit
    widgetSetSizeRequest window w h
    widgetShowAll window
    dw <- widgetGetDrawWindow window
    gc <- gcNew dw
    pb <- createPixbuf (ncols m) (nrows m)
    setPixels pb m
    let draw = do { drawPixbuf dw gc pb srcX srcY destX destY srcWidth srcHeight dither xDither yDither
                  ; return True }
    timeoutAdd draw 500
    widgetShowAll window
    mainGUI
    where
        srcX = 0
        srcY = 0
        destX = 0
        destY = 0
        srcWidth = -1
        srcHeight = -1
        dither = RgbDitherNone
        xDither = 0
        yDither = 0

module Matrix2Image where

import Data.Array.MArray
import Data.Matrix
import Data.Word

import Graphics.UI.Gtk (eventWindow)
import Graphics.UI.Gtk.Abstract.Widget (widgetGetDrawWindow, widgetShowAll)
import Graphics.UI.Gtk.General.General (initGUI, mainGUI)
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Windows.Window (windowNew)

lol :: IO Pixbuf
lol = pixbufNew ColorspaceRgb False 8 32 32

setRGBA :: Pixbuf -> Int -> Int -> (Word8, Word8, Word8, Word8) -> IO ()
setRGBA pb x y (r, g, b, a) = do
    ps <- pixbufGetPixels pb :: IO (PixbufData Int Word8)
    row <- pixbufGetRowstride pb
    writeArray ps (y*row + 4*x + 0) r
    writeArray ps (y*row + 4*x + 1) g
    writeArray ps (y*row + 4*x + 2) b
    writeArray ps (y*row + 4*x + 3) a

setPixels :: RealFrac a => Pixbuf -> Matrix a -> IO [()]
setPixels pb m = do
                     sequence [ f x y $ m ! (x, y) | y <- [0 .. (nrows m) - 1], x <- [0 .. (ncols m) - 1] ]
                     where
                         f x y val = do
                             let r = (round val :: Word8)
                             setRGBA pb x y (128, 128, 200, 128)

drawSmth :: RealFrac a => Matrix a -> IO ()
drawSmth m = do
               initGUI
               w <- windowNew
               widgetShowAll w
               mainGUI
               dw <- widgetGetDrawWindow w
               gc <- gcNew dw
               pb <- lol
               setPixels pb m
               drawPixbuf dw gc pb srcX srcY destX destY srcWidth srcHeight dither xDither yDither
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

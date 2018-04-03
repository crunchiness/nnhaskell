module Matrix2Image where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk (eventWindow)
import Graphics.UI.Gtk.Windows.Window (windowNew)
import Graphics.UI.Gtk.Abstract.Widget (widgetGetDrawWindow)

lol :: IO Pixbuf
lol = pixbufNew ColorspaceRgb False 8 32 32

-- setRGBA pb x y (r, g, b, a) = do
--     ps <- pixbufGetPixels pb :: IO (PixbufData Int Word8)
--     row <- pixbufGetRowstride pb
--     writeArray ps (y*row + 4*x + 0) r
--     writeArray ps (y*row + 4*x + 1) g
--     writeArray ps (y*row + 4*x + 2) b
--     writeArray ps (y*row + 4*x + 3) a

drawSmth :: IO ()
drawSmth = do
               w <- windowNew
               dw <- widgetGetDrawWindow w
               gc <- gcNew dw
               pb <- lol
               drawPixbuf dw gc pb srcX srcY destX destY srcWidth srcHeight dither xDither yDither
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

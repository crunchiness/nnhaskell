module Matrix2Image where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk (eventWindow)
import Graphics.UI.Gtk.Windows.Window (windowNew)
import Graphics.UI.Gtk.Abstract.Widget (widgetGetDrawWindow)

lol :: IO Pixbuf
lol = pixbufNew ColorspaceRgb False 8 32 32

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

module Emulator.Video.Display where

import Emulator.Memory
import Emulator.Video.BitmapModes
import Emulator.Video.TileModes
import Emulator.Video.Util
import Emulator.Video.VideoController

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

display :: IO ()
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

animate :: GLUT.IdleCallback
animate = do
  clear [GLUT.ColorBuffer]
  _ <- drawBg "/Users/harryduce/4thYrProj/bmp/toad.bmp" (150, 230) (100, 160)
  GLUT.swapBuffers

backgroundMode :: AddressSpace m => m a
backgroundMode = do
  record <- recordLCDControl
  if bgMode record <= 2 then
    tileModes record
  else
    bitmapModes record

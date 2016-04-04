module Emulator.Video.Display where

import Emulator.Memory
import Emulator.Video.BitmapModes
import Emulator.Video.TileModes
--import Emulator.Video.Util
import Emulator.Video.VideoController

import Emulator.Interpreter

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

display :: IO ()
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

animate :: TMVar SystemState -> GLUT.IdleCallback
animate chan = do
  liftIO $ print "waiting"
  mem <- atomically $ takeTMVar chan
  void $ flip runSystemT mem $ do
    readAddressWord 0x08000000 >>= liftIO . print
    liftIO $ clear [GLUT.ColorBuffer]
    liftIO $ GLUT.swapBuffers

backgroundMode :: AddressSpace m => m ()
backgroundMode = do
  record <- recordLCDControl
  if bgMode record <= 2 then
    tileModes record
  else
    bitmapModes record

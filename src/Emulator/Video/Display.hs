module Emulator.Video.Display where

import Emulator.Memory
import Emulator.Video.BitmapModes
import Emulator.Video.Sprite
import Emulator.Video.TileMode
import Emulator.Video.VideoController

import Emulator.Interpreter.Monad

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

display :: IO ()
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

animate' :: TMVar SystemState -> GLUT.IdleCallback
animate' chan = do
  liftIO $ putStrLn "waiting"
  mem <- atomically $ takeTMVar chan
  void $ flip runSystemT mem $ do
    --readAddressWord 0x08000000 >>= liftIO . print
    liftIO $ clear [GLUT.ColorBuffer]
    record <- recordLCDControl
    backgroundMode record
    readOAM $ objCharacterVRAMMapping record
    liftIO $ GLUT.swapBuffers

animate :: TMVar SystemState -> GLUT.IdleCallback
animate _ = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

backgroundMode :: (AddressSpace m, MonadIO m) => LCDControl -> m ()
backgroundMode record = do
  if bgMode record <= 2 then
    tileModes record
  else
    bitmapModes record

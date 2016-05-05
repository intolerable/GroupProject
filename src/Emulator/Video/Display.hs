module Emulator.Video.Display where

--import Emulator.Video.BitmapModes
import Emulator.Video.Sprite
import Emulator.Video.TileMode
import Emulator.Video.VideoController

import Emulator.Interpreter.Monad

import Control.Concurrent.STM
import Control.Concurrent.STM.TXChan
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

display :: IO ()
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

animate' :: TXChan SystemState -> GLUT.IdleCallback
animate' chan = do
  liftIO $ putStrLn "waiting"
  mem <- atomically $ takeTXChan chan
  liftIO $ putStrLn "got memory"
  void $ flip runSystemT mem $ do
    --readAddressWord 0x08000000 >>= liftIO . print
    liftIO $ clear [GLUT.ColorBuffer]
    record <- recordLCDControl
    _backgrounds <- if bgMode record <= 2 then tileModes record else tileModes record
    readOAM $ objCharacterVRAMMapping record
    liftIO $ GLUT.swapBuffers

animate :: TXChan SystemState -> GLUT.IdleCallback
animate _ = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

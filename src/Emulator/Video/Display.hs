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
    _sprites <- readOAM $ objCharacterVRAMMapping record
    liftIO $ GLUT.swapBuffers

animate :: TXChan SystemState -> GLUT.IdleCallback
animate _ = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

-- right end of list will be drawn last therefore will appear on top
-- left = low, right = high priority
comparePrio :: ScreenObj -> ScreenObj -> Ordering
comparePrio (BG _ prio1 layer1) (BG _ prio2 layer2)
  | prio1 < prio2 = GT
  | prio1 > prio2 = LT
  | otherwise = if layer1 < layer2 then GT else LT
comparePrio (Sprite _ prio1) (Sprite _ prio2)
  | prio1 < prio2 = GT
  | prio1 > prio2 = LT
  | otherwise = EQ
comparePrio (BG _ prio1 _) (Sprite _ prio2)
  | prio1 < prio2 = GT
  | prio1 > prio2 = LT
  | otherwise = LT
comparePrio (Sprite _ prio1) (BG _ prio2 _)
  | prio1 < prio2 = GT
  | prio1 > prio2 = LT
  | otherwise = GT
comparePrio _ _ = EQ

module Emulator.Video.Display where

import Emulator.Memory
import Emulator.Video.BitmapModes
import Emulator.Video.Renderer
import Emulator.Video.Sprite
import Emulator.Video.TileMode
import Emulator.Video.Util
import Emulator.Video.VideoController

import Emulator.Interpreter.Monad

import Control.Concurrent.STM
import Control.Concurrent.STM.TXChan
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

display :: IO ()
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

animate :: TXChan SystemState -> GLUT.IdleCallback
animate chan = do
  liftIO $ putStrLn "waiting"
  mem <- atomically $ takeTXChan chan
  liftIO $ putStrLn "got memory"
  void $ flip runSystemT mem $ do
    --readAddressWord 0x08000000 >>= liftIO . print
    liftIO $ clear [GLUT.ColorBuffer]
    record <- recordLCDControl
    backgrounds <- if bgMode record <= 2 then tileModes record else bitmapModes record
    sprites <- displaySprites record
    let screenObjs = sortBy comparePrio (sprites ++ backgrounds)
    liftIO $ renderScreenObj screenObjs
    liftIO $ GLUT.swapBuffers

displaySprites :: AddressSpace m => LCDControl -> m [ScreenObj]
displaySprites cnt
  | screenDispBGOBJ cnt = readOAM $ objCharacterVRAMMapping cnt
  | otherwise = return [Hidden]

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

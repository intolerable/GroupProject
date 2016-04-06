module Emulator
  ( main
  , loadROM ) where

import Emulator.CPU
import Emulator.CPU.Instructions.ARM
import Emulator.CPU.Instructions.ARM.Parser
import Emulator.CPU.Instructions.Types
import Emulator.Interpreter
import Emulator.ROM
import Emulator.ROM.Parser
import Emulator.Types
import Emulator.Video.Display

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Graphics.Rendering.OpenGL
import qualified Data.ByteString as BS
import qualified Graphics.UI.GLUT as GLUT

-- | Launches the emulator. Currently only initializes GL and fires off the main loop.
--     In the future, this will parse command line arguments so that the emulator
--     can be run in headless mode \/ debug mode \/ with various other options.
main :: IO ()
main = do
  initGL
  GLUT.mainLoop

-- | Initialize OpenGL and set up the window ready to render sprite- and tile-based 2D
--     graphics. We enable double-buffering and alpha modes, create a window with a
--     modified ortho (in order to match GBA device pixels to GL units), and then
--     link the functions for the display and idle loops.
initGL :: IO ()
initGL = do
  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $=
    [ GLUT.RGBAMode
    , GLUT.WithAlphaComponent
    , GLUT.DoubleBuffered ]
  _ <- GLUT.createWindow "GBA"
  GLUT.windowSize $= Size 240 160
  GLUT.viewport $= (Position 0 0, Size 240 160)
  GLUT.reshapeCallback $= Just reshape
  GLUT.clearColor $= Color4 0 0 0 0
  GLUT.displayCallback $= display
  chan <- newEmptyTMVarIO
  GLUT.idleCallback $= Just (animate chan)

-- | Callback that should be executed whenever the window is resized. We just fix the
--     ortho to 240x160 and set the correct window size.
reshape :: GLUT.ReshapeCallback
reshape (Size x y) = do
  loadIdentity
  ortho 0 240 160 0 0 1
  viewport $= (GLUT.Position 0 0, Size x y)

-- | Load a ROM from a given file path, and then start executing the ROM.
loadROM :: FilePath -> IO ()
loadROM fp =
  readROM fp >>= \case
    Left err -> putStrLn err
    Right (rh, _, bs) ->
      case parseARM (mwordFromBS (rh ^. startLocation)) of
        Right (AL, Branch (Link False) _) ->
          void $ runSystemT interpretLoop $
            buildInitialState bs
        _ -> error "loadROM: undefined"

mwordFromBS :: ByteString -> MWord
mwordFromBS bs =
  case map fromIntegral $ BS.unpack bs of
    [b1, b2, b3, b4] ->
      -- little endian
      b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24)
    _ -> 0

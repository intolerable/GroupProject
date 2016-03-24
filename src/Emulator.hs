module Emulator where

import Emulator.CPU
import Emulator.CPU.Instructions.Parser
import Emulator.Interpreter
import Emulator.ROM
import Emulator.ROM.Parser
import Emulator.Types
import Emulator.Video.Display

import Control.Lens
import Data.Bits
import Data.ByteString (ByteString)
import Graphics.Rendering.OpenGL
import Control.Monad.Trans.State
import qualified Data.ByteString as BS
import qualified Graphics.UI.GLUT as GLUT

main :: IO ()
main = do
  initGL
  GLUT.mainLoop

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
  GLUT.clearColor $= Color4 0 0 0 0
  GLUT.displayCallback $= display
  GLUT.idleCallback $= Just animate

loadROM :: FilePath -> IO ()
loadROM fp = do
  readROM fp >>= \case
    Left err -> putStrLn err
    Right (rh, _, bs) ->
      case parseARM (mwordFromBS (rh ^. startLocation)) of
        Right (AL, Branch (Link False) _) ->
          evalStateT (runSystem interpretLoop) $
            buildInitialState bs
        _ -> error "loadROM: undefined"

mwordFromBS :: ByteString -> MWord
mwordFromBS bs =
  case map fromIntegral $ BS.unpack bs of
    [b1, b2, b3, b4] ->
      -- little endian
      b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24)
    _ -> 0

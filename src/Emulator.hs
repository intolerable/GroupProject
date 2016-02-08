module Emulator where

import Graphics.Rendering.OpenGL
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
  GLUT.displayCallback $= do
    clear [GLUT.ColorBuffer]
    GLUT.swapBuffers

module Emulator.Video.Display where

import Graphics.Rendering.OpenGL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLUT as GLUT

display :: IO ()
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

animate :: GLUT.IdleCallback
animate = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

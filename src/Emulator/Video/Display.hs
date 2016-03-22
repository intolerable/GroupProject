module Emulator.Video.Display where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

display :: GLUT.IdleCallback
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

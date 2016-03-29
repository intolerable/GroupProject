module Emulator.Video.Display where

import Codec.Picture.Bitmap
import Data.Array.Storable
import Data.Word
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

type FileName = String

display :: IO ()
display = do
  clear [GLUT.ColorBuffer]
  GLUT.swapBuffers

animate :: GLUT.IdleCallback
animate = do
  clear [GLUT.ColorBuffer]
  name <- loadTestTexture "/Users/harryduce/Downloads/LAND3.bmp"
  textureBinding Texture2D $= Just name
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  renderPrimitive Quads $ do
    texCoord $ TexCoord2 0 (0 :: GLdouble)
    vertex $ Vertex2 0 (0 :: GLdouble)
    texCoord $ TexCoord2 1 (0 :: GLdouble)
    vertex $ Vertex2 1 (0 :: GLdouble)
    texCoord $ TexCoord2 1 (1 :: GLdouble)
    vertex $ Vertex2 1 (1 :: GLdouble)
    texCoord $ TexCoord2 0 (1 :: GLdouble)
    vertex $ Vertex2 0 (1 :: GLdouble)
  GLUT.swapBuffers

loadTestTexture :: FileName -> IO TextureObject
loadTestTexture fname = undefined

loadTexture :: StorableArray Int Word32 -> Int -> Int -> IO TextureObject
loadTexture arr w h = withStorableArray arr $ \ptr -> do
    name <- genObjectName
    textureBinding Texture2D $= Just name
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData RGBA UnsignedByte ptr)
    return name

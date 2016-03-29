module Emulator.Video.Display where

import Codec.Picture
import Data.Vector.Storable
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
  name <- loadTestTexture "/Users/harryduce/Downloads/star.bmp"
  textureBinding Texture2D $= Just name
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  renderPrimitive Quads $ do
    texCoord $ TexCoord2 0 (1 :: GLdouble)
    vertex $ Vertex2 0 (127 :: GLdouble)
    texCoord $ TexCoord2 0 (0 :: GLdouble)
    vertex $ Vertex2 0 (0 :: GLdouble)
    texCoord $ TexCoord2 1 (0 :: GLdouble)
    vertex $ Vertex2 127 (0 :: GLdouble)
    texCoord $ TexCoord2 1 (1 :: GLdouble)
    vertex $ Vertex2 127 (127 :: GLdouble)
  GLUT.swapBuffers

-- Temp. function for reading in BMP files
loadTestTexture :: FileName -> IO TextureObject
loadTestTexture fname = do
  bmp <- readBitmap fname
  case bmp of
    Right bmp' -> do
      let (w, h, pixVec) = parseImage bmp'
      loadTexture pixVec w h
    Left _ -> undefined lines
  where
    parseImage :: DynamicImage -> (Int, Int, Vector Word8)
    parseImage (ImageRGB8 (Image w h pixData)) = do
      (w, h, pixData)
    parseImage _ = undefined lines

-- Takes Storable Vector8 and loads it to a 2D Texture
loadTexture :: Vector Word8 -> Int -> Int -> IO TextureObject
loadTexture vec w h = unsafeWith vec $ \ptr -> do
    name <- genObjectName
    textureBinding Texture2D $= Just name
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData RGB UnsignedByte ptr)
    return name

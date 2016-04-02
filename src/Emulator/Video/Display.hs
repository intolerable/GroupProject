module Emulator.Video.Display where

import Emulator.Memory
import Emulator.Video.BitmapModes
import Emulator.Video.TileModes
import Emulator.Video.VideoController
import Emulator.Types

import Codec.Picture
import Data.Vector.Storable
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
  _ <- drawBg "/Users/harryduce/4thYrProj/bmp/toad.bmp" (150, 230) (100, 160)
  GLUT.swapBuffers

backgroundMode :: AddressSpace m => m a
backgroundMode = do
  record <- recordLCDControl
  if bgMode record <= 2 then
    tileModes record
  else
    bitmapModes record

drawBg :: FileName -> (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> IO TextureObject
drawBg fname (x1, x2) (y1, y2) = do
  bg <- loadTestTexture fname
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  renderPrimitive Quads $ do
    texCoord $ TexCoord2 0 (0 :: GLdouble)
    vertex $ Vertex2 x1 (y1 :: GLdouble)
    texCoord $ TexCoord2 1 (0 :: GLdouble)
    vertex $ Vertex2 x2 (y1 :: GLdouble)
    texCoord $ TexCoord2 1 (1 :: GLdouble)
    vertex $ Vertex2 x2 (y2 :: GLdouble)
    texCoord $ TexCoord2 0 (1 :: GLdouble)
    vertex $ Vertex2 x1 (y2 :: GLdouble)
  return bg

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
    parseImage :: DynamicImage -> (Int, Int, Vector Byte)
    parseImage (ImageRGB8 (Image w h pixData)) = do
      (w, h, pixData)
    parseImage _ = undefined lines

-- Takes Storable Vector8 and loads it to a 2D Texture
-- Assigns textureBinding to texture object
loadTexture :: Vector Byte -> Int -> Int -> IO TextureObject
loadTexture vec w h = unsafeWith vec $ \ptr -> do
    name <- genObjectName
    textureBinding Texture2D $= Just name
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData RGB UnsignedByte ptr)
    return name

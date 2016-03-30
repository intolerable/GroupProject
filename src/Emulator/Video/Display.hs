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
  bg0 <- drawBackground "/Users/harryduce/4thYrProj/bmp/BLU.bmp" (0, 240) (0, 160)
  bg1 <- drawBackground "/Users/harryduce/4thYrProj/bmp/RED.bmp" (0, 240) (30, 160)
  bg2 <- drawBackground "/Users/harryduce/4thYrProj/bmp/GRN.bmp" (0, 240) (60, 160)
  obj <- drawBackground "/Users/harryduce/4thYrProj/bmp/paper_mario.bmp" (50, 80) (120, 160)
  obj2 <- drawBackground "/Users/harryduce/4thYrProj/bmp/toad.bmp" (150, 230) (100, 160)
  GLUT.swapBuffers

drawBackground :: FileName -> (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> IO TextureObject
drawBackground fname (x1, x2) (y1, y2) = do
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
    parseImage :: DynamicImage -> (Int, Int, Vector Word8)
    parseImage (ImageRGB8 (Image w h pixData)) = do
      (w, h, pixData)
    parseImage _ = undefined lines

-- Takes Storable Vector8 and loads it to a 2D Texture
-- Assigns textureBinding to texture object
loadTexture :: Vector Word8 -> Int -> Int -> IO TextureObject
loadTexture vec w h = unsafeWith vec $ \ptr -> do
    name <- genObjectName
    textureBinding Texture2D $= Just name
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData RGB UnsignedByte ptr)
    return name

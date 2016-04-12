module Emulator.Video.Util where

import Emulator.Types

import Codec.Picture
import Data.Array.Storable
import Data.Vector.Storable
import Graphics.Rendering.OpenGL

type FileName = String

drawTile :: StorableArray Address Byte -> (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> IO TextureObject
drawTile _ _ _ = undefined

drawTile' :: FileName -> (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> IO TextureObject
drawTile' fname (x1, x2) (y1, y2) = do
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

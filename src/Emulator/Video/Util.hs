module Emulator.Video.Util where

import Emulator.Memory
import Emulator.Types

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Array.Storable
import Data.Bits
import Graphics.Rendering.OpenGL

type AddressIO m = (AddressSpace m, MonadIO m)
type PixFormat = Bool
type TileOffset = (GLdouble, GLdouble)
type TileSet = Array Address Byte

drawTile :: StorableArray Address HalfWord -> (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> IO ()
drawTile arr (x1, x2) (y1, y2) = do
  _ <- liftIO $ loadTexture arr
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
  return ()

loadTexture :: StorableArray Address HalfWord -> IO TextureObject
loadTexture arr = withStorableArray arr $ \ptr -> do
    tile <- genObjectName
    textureBinding Texture2D $= Just tile
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D 8 8) 0 (PixelData RGB UnsignedByte ptr)
    return tile

bytesToHalfWord :: Byte -> Byte -> HalfWord
bytesToHalfWord lower upper = ((fromIntegral upper :: HalfWord) `shiftL` 8) .|. ((fromIntegral lower :: HalfWord) .&. 0xFF) :: HalfWord

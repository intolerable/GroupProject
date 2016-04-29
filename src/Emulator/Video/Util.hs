module Emulator.Video.Util where

import Emulator.Memory
import Emulator.Types

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Array.Storable
import Data.Bits
import Graphics.Rendering.OpenGL

type AddressIO m = (AddressSpace m, MonadIO m)
type AffineParameters = (GLdouble, GLdouble, GLdouble, GLdouble)
type PixFormat = Bool
type QuadCoords = ((GLdouble, GLdouble), (GLdouble, GLdouble), (GLdouble, GLdouble), (GLdouble, GLdouble))
type Tile = Array Address Byte
type TileOffset = (GLdouble, GLdouble)
type TileSet = Array Address Byte
type TileSetBaseAddress = Address

drawTile :: StorableArray Address HalfWord -> QuadCoords -> IO ()
drawTile arr ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) = do
  _ <- liftIO $ loadTexture arr
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  renderPrimitive Quads $ do
    texCoord $ TexCoord2 0 (0 :: GLdouble)
    vertex $ Vertex2 x1 (y1 :: GLdouble)
    texCoord $ TexCoord2 1 (0 :: GLdouble)
    vertex $ Vertex2 x2 (y2 :: GLdouble)
    texCoord $ TexCoord2 1 (1 :: GLdouble)
    vertex $ Vertex2 x4 (y4 :: GLdouble)
    texCoord $ TexCoord2 0 (1 :: GLdouble)
    vertex $ Vertex2 x3 (y3 :: GLdouble)

-- Affine transformation
affineCoords :: TileOffset -> (GLdouble, GLdouble) -> AffineParameters -> QuadCoords
affineCoords (xOff, yOff) (xCentre, yCentre) (pa, pb, pc, pd) = ((x1, y1), (x2, y2), (x3, y3), (x4, y4))
  where
    x1 = (pa * (xOff - xCentre)) + (pb * (yOff - yCentre)) + xCentre
    y1 = (pc * (xOff - xCentre)) + (pd * (yOff - yCentre)) + yCentre
    x2 = (pa * ((xOff + 8) - xCentre)) + (pb * (yOff - yCentre)) + xCentre
    y2 = (pc * ((xOff + 8) - xCentre)) + (pd * (yOff - yCentre)) + yCentre
    x3 = (pa * (xOff - xCentre)) + pb * ((yOff + 8) - yCentre) + xCentre
    y3 = (pc * (xOff - xCentre)) + pd * ((yOff + 8) - yCentre) + yCentre
    x4 = (pa * ((xOff + 8) - xCentre)) + (pb * ((yOff + 8) - yCentre)) + xCentre
    y4 = (pc * ((xOff + 8) - xCentre)) + (pd * ((yOff + 8) - yCentre)) + yCentre

loadTexture :: StorableArray Address HalfWord -> IO TextureObject
loadTexture arr = withStorableArray arr $ \ptr -> do
    tile <- genObjectName
    textureBinding Texture2D $= Just tile
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D 8 8) 0 (PixelData RGB UnsignedByte ptr)
    return tile

bytesToHalfWord :: Byte -> Byte -> HalfWord
bytesToHalfWord lower upper = ((fromIntegral upper :: HalfWord) `shiftL` 8) .|. ((fromIntegral lower :: HalfWord) .&. 0xFF) :: HalfWord

-- If pixel format is 8bpp then the tileIndex read from the map is in steps of 40h
-- If pixel format is 4bpp then the tileIndex read from the map is in steps of 20h
convIntToAddr :: Int -> PixFormat -> Address
convIntToAddr 0 _ = 0x00000000
convIntToAddr n True = (0x00000040 * fromIntegral n)
convIntToAddr n _ = (0x00000020 * fromIntegral n)

-- If pixel format is 8bpp then TileSet is read in chunks of 40h
-- If not then TileSet is read in chunks of 20h
getTile :: PixFormat -> Address -> TileSet -> Tile
getTile True tileIdx tileSet = (ixmap (tileIdx, tileIdx + 0x0000003F) (id) tileSet :: Tile)
getTile _ tileIdx tileSet = (ixmap (tileIdx, tileIdx + 0x0000001F) (id) tileSet :: Tile)

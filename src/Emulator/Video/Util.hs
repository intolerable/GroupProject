module Emulator.Video.Util where

import Emulator.Memory
import Emulator.Types

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Array.Storable
import Data.Bits
import Graphics.Rendering.OpenGL
import Utilities.Parser.TemplateHaskell

type AddressIO m = (AddressSpace m, MonadIO m)
type AffineParameters = (GLdouble, GLdouble, GLdouble, GLdouble)
type AffineRefPoints = (GLdouble, GLdouble)
type PixFormat = Bool
type QuadCoords = ((GLdouble, GLdouble), (GLdouble, GLdouble), (GLdouble, GLdouble), (GLdouble, GLdouble))
type Tile = Array Address Byte
type TileMapBaseAddress = Address
type TileOffset = (GLdouble, GLdouble)
type TileSet = Array Address Byte
type TileSetBaseAddress = Address

-- drawTile :: StorableArray Address HalfWord -> QuadCoords -> IO ()
-- drawTile arr coords = drawTile' arr coords UnsignedByte

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
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D 8 8) 0 (PixelData RGB UnsignedShort ptr)
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

convToFixedNum :: Byte -> Byte -> GLdouble
convToFixedNum low up
  | sign = negate val
  | otherwise = val
  where
    val =  intPor + (fracPor / 256.0)
    hword = bytesToHalfWord low up
    fracPor = fromIntegral $ $(bitmask 7 0) hword
    intPor = fromIntegral $ $(bitmask 14 8) hword
    sign = testBit hword 15

referencePoint :: MWord -> GLdouble
referencePoint word
  | sign = negate val
  | otherwise = val
  where
    val = intPor + (frac / 256)
    frac = fromIntegral $ $(bitmask 7 0) word :: GLdouble
    intPor = fromIntegral $ $(bitmask 26 8) word :: GLdouble
    sign = testBit word 27

affineParameters :: Address -> Address -> Address -> Address -> Array Address Byte -> AffineParameters
affineParameters addr0 addr1 addr2 addr3 mem = (pa, pb, pc, pd)
  where
    pa = convToFixedNum (mem!(addr0)) (mem!(addr0 + 0x00000001))
    pb = convToFixedNum (mem!(addr1)) (mem!(addr1 + 0x00000001))
    pc = convToFixedNum (mem!(addr2)) (mem!(addr2 + 0x00000001))
    pd = convToFixedNum (mem!(addr3)) (mem!(addr3 + 0x00000001))

data BGControl =       -- R/W. BGs 0-3
  BGControl { bgPriority :: Int          -- 0 = Highest
            , characterBaseBlock :: TileSetBaseAddress  -- =BG Tile Data. Indicates the start of tile counting
            , mosaic :: Bool
            , colorsPalettes :: Bool      -- (0=16/16, 1=256/1)
            , screenBaseBlock :: TileMapBaseAddress
            , displayAreaFlow :: Bool     -- BG 2 & BG 3 only
            , screenSize :: Int }
  deriving (Show, Read, Eq)

-- Reads a mem address that points to bgcnt register
recordBGControl :: AddressSpace m => Address -> m BGControl
recordBGControl addr = do
  hword <- readAddressHalfWord addr
  let bgCNT = BGControl (fromIntegral $ $(bitmask 1 0) hword)
                        (baseTileSetAddr (fromIntegral $ $(bitmask 3 2) hword))
                        (testBit hword 6)
                        (testBit hword 7)
                        (baseTileMapAddr (fromIntegral $ $(bitmask 12 8) hword))
                        (testBit hword 13)
                        (fromIntegral $ $(bitmask 15 14) hword)
  return bgCNT

-- Gets the base memory addres for the tile
baseTileSetAddr :: Byte -> TileSetBaseAddress
baseTileSetAddr tileBase = 0x06000000 + (0x00004000 * (fromIntegral tileBase))

baseTileMapAddr :: Byte -> TileMapBaseAddress
baseTileMapAddr mapBase = 0x06000000 + (0x00000800 * (fromIntegral mapBase))

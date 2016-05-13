module Emulator.Video.Util where

import Emulator.Memory
import Emulator.Types

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Bits
import Graphics.Rendering.OpenGL
import Utilities.Parser.TemplateHaskell

data ScreenObj =
  BG [Tile] Priority Layer |
  Sprite [Tile] Priority |
  Hidden
  deriving (Show, Read, Eq)

data Tile = Tile [HalfWord] QuadCoords
  deriving (Show, Read, Eq)

type Layer = Int
type Priority = Int

type AddressIO m = (AddressSpace m, MonadIO m)
type AffineParameters = (GLdouble, GLdouble, GLdouble, GLdouble)
type AffineRefPoints = (GLdouble, GLdouble)
type PaletteFormat = Bool
type QuadCoords = ((GLdouble, GLdouble), (GLdouble, GLdouble), (GLdouble, GLdouble), (GLdouble, GLdouble))
type PixData = Array Address Byte
type TileMapBaseAddress = Address
type TileOffset = (GLdouble, GLdouble)
type TileSet = Array Address Byte
type TileSetBaseAddress = Address
type Centre = (GLdouble, GLdouble)

transformCoords :: [Tile] -> Centre -> AffineParameters -> [Tile]
transformCoords [] _ _ = []
transformCoords ((Tile pix coords):xs) centre params = Tile pix affCoords:transformCoords xs centre params
  where
    affCoords = affineCoords centre params coords

-- this will be the function that is used
affineCoords :: Centre -> AffineParameters -> QuadCoords -> QuadCoords
affineCoords (xCentre, yCentre) (pa, pb, pc, pd) coords = ((affx1, affy1), (affx2, affy2), (affx3, affy3), (affx4, affy4))
  where
    ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) = coords
    affx1 = (pa * (x1 - xCentre)) + (pb * (y1 - yCentre)) + xCentre
    affy1 = (pc * (x1 - xCentre)) + (pd * (y1 - yCentre)) + yCentre
    affx2 = (pa * (x2 - xCentre)) + (pb * (y2 - yCentre)) + xCentre
    affy2 = (pc * (x2 - xCentre)) + (pd * (y2 - yCentre)) + yCentre
    affx3 = (pa * (x3 - xCentre)) + (pb * (y3 - yCentre)) + xCentre
    affy3 = (pc * (x3 - xCentre)) + (pd * (y3 - yCentre)) + yCentre
    affx4 = (pa * (x4 - xCentre)) + (pb * (y4 - yCentre)) + xCentre
    affy4 = (pc * (x4 - xCentre)) + (pd * (y4 - yCentre)) + yCentre

bytesToHalfWord :: Byte -> Byte -> HalfWord
bytesToHalfWord lower upper = ((fromIntegral upper :: HalfWord) `shiftL` 8) .|. ((fromIntegral lower :: HalfWord) .&. 0xFF) :: HalfWord

-- If pixel format is 8bpp then the tileIndex read from the map is in steps of 40h
-- If pixel format is 4bpp then the tileIndex read from the map is in steps of 20h
convIntToAddr :: Int -> PaletteFormat -> Address
convIntToAddr n True = (0x00000040 * fromIntegral n)
convIntToAddr n _ = (0x00000020 * fromIntegral n)

-- If pixel format is 8bpp then TileSet is read in chunks of 40h
-- If not then TileSet is read in chunks of 20h
getTile :: PaletteFormat -> Address -> TileSet -> PixData
getTile True tileIdx tileSet = (ixmap (tileIdx, tileIdx + 0x0000003F) (id) tileSet :: PixData)
getTile _ tileIdx tileSet = (ixmap (tileIdx, tileIdx + 0x0000001F) (id) tileSet :: PixData)

convToFixedNum :: Byte -> Byte -> GLdouble
convToFixedNum low up
  | sign = negate val
  | otherwise = val
  where
    val =  intPor + (fracPor / 256)
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

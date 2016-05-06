module Emulator.Video.Renderer where

import Emulator.Types
import Emulator.Video.Util

import Control.Monad.IO.Class
import Data.Array.Storable
import Graphics.Rendering.OpenGL

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

loadTexture :: StorableArray Address HalfWord -> IO TextureObject
loadTexture arr = withStorableArray arr $ \ptr -> do
    tile <- genObjectName
    textureBinding Texture2D $= Just tile
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D 8 8) 0 (PixelData RGB UnsignedShort ptr)
    return tile
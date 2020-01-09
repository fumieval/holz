---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Low level interface for font rendering
---------------------------------------------------------------------------
module Graphics.Holz.Font
  ( Font
  , readFont
  , renderChar
  ) where

import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Vector.Storable as V
import Linear
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import qualified Graphics.Rendering.FreeType.Internal.Vector as V
import Graphics.Rendering.FreeType.Internal.Bitmap as B
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT
import Graphics.Rendering.FreeType.Internal.Face as F
import Graphics.Rendering.FreeType.Internal.Library as Ls
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe
import Codec.Picture

-- | Font object
newtype Font = Font FT_Face

-- | Create a 'Font' from the given file.
readFont :: MonadIO m => FilePath -> m Font
readFont path = liftIO $ alloca $ \p -> do
    runFreeType $ withCString path $ \str -> ft_New_Face freeType str 0 p
    f <- peek p
    return $ Font f

runFreeType :: IO CInt -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" Prelude.++ show r

freeType :: FT_Library
freeType = unsafePerformIO $ alloca $ \p -> do
  runFreeType $ ft_Init_FreeType p
  peek p

-- | Render a character. It also returns the offset and advance.
renderChar :: MonadIO m => Font -> Float -> Char -> m (Image PixelRGBA8, V2 Float, V2 Float)
renderChar (Font face) pixel ch = liftIO $ do
  runFreeType $ ft_Set_Pixel_Sizes face 0 (floor pixel)

  runFreeType $ ft_Load_Char face (fromIntegral $ fromEnum ch) ft_LOAD_RENDER

  slot <- peek $ glyph face

  bmp <- peek $ GS.bitmap slot
  left <- fmap fromIntegral $ peek $ GS.bitmap_left slot
  top <- fmap fromIntegral $ peek $ GS.bitmap_top slot

  let h = fromIntegral $ B.rows bmp
      w = fromIntegral $ B.width bmp

  fptr <- newForeignPtr_ $ castPtr $ buffer bmp

  adv <- peek $ GS.advance slot
  let b = fromColorAndOpacity (PixelRGB8 255 255 255)
        $ Image w h $ V.unsafeFromForeignPtr0 fptr $ h * w
  return (b
    , V2 left (-top)
    , V2 (fromIntegral (V.x adv) / 64) 0)

fromColorAndOpacity :: PixelRGB8 -> Image Pixel8 -> Image PixelRGBA8
fromColorAndOpacity (PixelRGB8 r g b) (Image w h vec) = generateImage pix w h where
  pix i j = PixelRGBA8 r g b $ V.unsafeIndex vec $ i + j * w
  {-# INLINE pix #-}

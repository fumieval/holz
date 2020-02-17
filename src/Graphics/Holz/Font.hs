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
import qualified Data.ByteString.Internal as B
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
renderChar :: MonadIO m => Font
  -> Int -- ^ size in pixels
  -> Char
  -> m (Image Pixel8, V2 Float, V2 Float)
renderChar (Font face) pixel ch = liftIO $ do
  runFreeType $ ft_Set_Pixel_Sizes face 0 $ fromIntegral pixel

  runFreeType $ ft_Load_Char face (fromIntegral $ fromEnum ch) ft_LOAD_RENDER

  slot <- peek $ glyph face

  bmp <- peek $ GS.bitmap slot
  left <- fmap fromIntegral $ peek $ GS.bitmap_left slot
  top <- fmap fromIntegral $ peek $ GS.bitmap_top slot

  let h = fromIntegral $ B.rows bmp
      w = fromIntegral $ B.width bmp

  fptr <- B.mallocByteString (h * w)
  withForeignPtr fptr $ \ptr -> B.memcpy ptr (castPtr (buffer bmp)) (h * w)

  let img = Image w h $ V.unsafeFromForeignPtr0 fptr $ h * w

  adv <- peek $ GS.advance slot
  return (img
    , V2 left (-top)
    , V2 (fromIntegral (V.x adv) / 64) 0)

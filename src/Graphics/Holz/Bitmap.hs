{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Holz.Bitmap (
  Bitmap(..)
  ,image
  ,hash
  ,liftImage
  ,liftImageIO
  ,size
  ,clip
  ,readFile
  ,writeFile
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Char
import Data.Monoid
import Language.Haskell.TH
import Linear
import Prelude hiding (readFile, writeFile)
import Codec.Picture as C
import Codec.Picture.Types as C
import qualified Data.BoundingBox as B
import qualified Data.Hashable as H
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Random
import Foreign.Marshal.Utils
import Foreign.Ptr

data Bitmap = Bitmap !(Image PixelRGBA8) !Int

image :: Lens' Bitmap (Image PixelRGBA8)
image f (Bitmap i h) = f i <&> \i' -> Bitmap i' h

hash :: Lens' Bitmap Int
hash f (Bitmap i h) = f h <&> \h' -> Bitmap i h'

clip :: Bitmap -> B.Box V2 Int -> Bitmap
clip (Bitmap (Image w0 _ vec) k) (B.Box (V2 x0 y0) (V2 x1 y1)) = Bitmap
  img
  (H.hash (x0, y0, x1, y1, k))
  where
    w = x1 - x0
    h = y1 - y0
    img = unsafePerformIO $ V.unsafeWith vec $ \ptr -> do
      mv <- MV.unsafeNew $ w * h * 4
      MV.unsafeWith mv $ \dst -> forM_ [0..h-1] $ \y ->
        copyBytes (plusPtr dst $ y * w * 4) (plusPtr ptr $ (*4) $ (y + y0) * w0 + x0) (4 * w)
      Image w h `fmap` V.unsafeFreeze mv

size :: Bitmap -> V2 Int
size (Bitmap (Image w h _) _) = V2 w h

liftImage :: Image PixelRGBA8 -> Bitmap
liftImage b@(Image _ _ r) = Bitmap b (V.foldl H.hashWithSalt 0 r)

liftImageIO :: MonadIO m => Image PixelRGBA8 -> m Bitmap
liftImageIO b = liftIO $ Bitmap b <$> randomIO

-- | Load an image file.
readFile :: MonadIO m => FilePath -> m Bitmap
readFile path = liftIO $ Bitmap <$> readImageRGBA8 path <*> randomIO

-- | Save 'Bitmap' into a file.
writeFile :: MonadIO m => FilePath -> Bitmap -> m ()
writeFile path (Bitmap p _) = liftIO $ writePng path p

fromDynamicImage :: DynamicImage -> Image PixelRGBA8
fromDynamicImage (ImageY8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageYA8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img
fromDynamicImage _ = error "Unsupported format"

readImageRGBA8 :: FilePath -> IO (Image PixelRGBA8)
readImageRGBA8 path = readImage path >>= either fail (return . fromDynamicImage)

class ToPixelRGBA8 a where
    toRGBA8 :: a -> PixelRGBA8

instance ToPixelRGBA8 Pixel8 where
    toRGBA8 b = PixelRGBA8 b b b 255

instance ToPixelRGBA8 PixelYA8 where
    toRGBA8 (PixelYA8 l a) = PixelRGBA8 l l l a

instance ToPixelRGBA8 PixelRGB8 where
    toRGBA8 (PixelRGB8 r g b) = PixelRGBA8 r g b 255

instance ToPixelRGBA8 PixelRGBA8 where
    toRGBA8 = id

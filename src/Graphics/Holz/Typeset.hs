{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Graphics.Holz.Typeset where

import Control.Monad.IO.Class
import Graphics.Holz.Font
import Graphics.Holz.Shader
import qualified Graphics.Holz.Shader.Simple as S
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as V
import Linear
import Data.IORef
import Data.Traversable
import Codec.Picture.Types
import UnliftIO.Resource

data Typeset = Typeset
  { font :: !Font
  , size :: !Int
  , atlas :: !(Texture Pixel8)
  , cursor :: !(IORef (V3 Int))
  , mapping :: !(IORef (IM.IntMap Glyph))
  , atlasSize :: !(V2 Int)
  }

data Glyph = Glyph
  { glyphOffset :: V2 Float
  , glyphAdvance :: V2 Float
  , glyphSize :: V2 Float
  , glyphTexCoords :: V2 (V2 Float)
  }

createTypeset :: MonadIO m => Font -> Int -> m Typeset
createTypeset font size = liftIO $ do
  let atlasSize = V2 2048 2048
  atlas <- createEmptyTexture atlasSize
  cursor <- newIORef zero
  mapping <- newIORef mempty
  pure Typeset{..}

char :: MonadIO m => Typeset -> Char -> m Glyph
char ts@Typeset{..} ch = liftIO $ do
  m <- readIORef mapping
  case IM.lookup (fromEnum ch) m of
    Just g -> pure g
    Nothing -> do
      g <- createGlyph ts ch
      atomicModifyIORef' mapping $ \x -> (IM.insert (fromEnum ch) g x, g)

createGlyph :: Typeset -> Char -> IO Glyph
createGlyph Typeset{..} ch = do
  (img, glyphOffset, glyphAdvance) <- renderChar font size ch
  let w = imageWidth img
  let h = imageHeight img
  let V2 atlasWidth _ = atlasSize
  tex0 <- atomicModifyIORef' cursor $ \(V3 x y next) -> (if x + w >= atlasWidth
    then V3 0 next 0
    else V3 (x + w) y $ max next (y + h), V2 x y)
  let tex1 = fmap fromIntegral (tex0 + V2 w h) / fmap fromIntegral atlasSize
      glyphTexCoords = V2 (fmap fromIntegral tex0 / fmap fromIntegral atlasSize) tex1
  writeTexture atlas tex0 img
  let glyphSize = fmap fromIntegral $ V2 w h
  return Glyph{..}

stringOn :: forall m. MonadResource m
  => (V2 Float -> V2 Float)
  -> Typeset
  -> S.RGBA
  -> String
  -> m S.Drawable
stringOn adjust ts@Typeset{..} color str = do
  glyphs <- traverse (char ts) str
  let (_, coords) = mapAccumL (\pos@(V2 _ baseY) (ch, g) -> case ch of
        '\n' -> (V2 0 (baseY + fromIntegral size * 1.5), [])
        _ -> (pos + glyphAdvance g
          , let base = pos + glyphOffset g
                conv v = let V2 x y = v + adjust (V2 0 0) in V3 x y 0
                V2 (V2 u0 v0) (V2 u1 v1) = glyphTexCoords g
                V2 w h = glyphSize g
            in genStrip
              [ S.fromV3RGBAUV (conv base) color (V2 u0 v0)
              , S.fromV3RGBAUV (conv $ base + V2 w 0) color (V2 u1 v0)
              , S.fromV3RGBAUV (conv $ base + V2 0 h) color (V2 u0 v1)
              , S.fromV3RGBAUV (conv $ base + V2 w h) color (V2 u1 v1)
              ])) (V2 0 0) $ zip str glyphs
  (_, vbuf) <- registerVertex Triangles $ V.fromList $ concat coords
  pure $ S.Drawable atlas (vbuf :: VertexBuffer S.Vertex)

genStrip :: [a] -> [a]
genStrip (x : y : zs) = go x y zs where
  go a b (c : cs) = a : b : c : go c b cs
  go _ _ [] = []
genStrip _ = error "genStrip: the list length must be equal or longer than 3"
{-# INLINE genStrip #-}

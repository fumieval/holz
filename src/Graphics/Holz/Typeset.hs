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
import Graphics.Holz.System
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as V
import Linear
import Data.IORef
import Data.Traversable
import Codec.Picture.Types
import GHC.Generics (Generic(..))

data Typeset = Typeset
  { font :: !Font
  , size :: !Int
  , atlas :: !Texture
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
  let atlasSize = V2 256 256
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
  print tex0
  writeTexture atlas tex0 img
  let glyphSize = fmap fromIntegral $ V2 w h
  return Glyph{..}

data Renderable v = Renderable !Texture !(VertexBuffer v)

class FromV2 v where
  fromV2 :: V2 Float -> V2 Float -> v

type IsVertex v = (Generic v, GVertex (Rep v), V.Storable v)

string :: forall m v. (Show v, MonadIO m, FromV2 v, IsVertex v) => Typeset -> String -> m (Renderable v)
string ts@Typeset{..} str = liftIO $ do
  glyphs <- traverse (char ts) str
  let (_, coords) = mapAccumL (\pos g -> (pos + glyphAdvance g
        , let base = pos + glyphOffset g
              V2 (V2 u0 v0) (V2 u1 v1) = glyphTexCoords g
              V2 w h = glyphSize g
          in genStrip
            [ fromV2 base (V2 u0 v0)
            , fromV2 (base + V2 w 0) (V2 u1 v0)
            , fromV2 (base + V2 0 h) (V2 u0 v1)
            , fromV2 (base + V2 w h) (V2 u1 v1)
            ])) (V2 0 0) glyphs
  print coords
  vbuf <- registerVertex Triangles $ V.fromList $ concat coords
  pure $ Renderable atlas vbuf

draw :: forall r m v. (HasShader r, S.HasModel (ShaderUniform r UniformVar), MonadHolz r m) => M44 Float -> Renderable v -> m ()
draw mat (Renderable tex buf) = do
  setUniform S.getModelVar mat
  drawVertexBuffer tex buf 

genStrip :: [a] -> [a]
genStrip (x : y : zs) = go x y zs where
  go a b (c : cs) = a : b : c : go c b cs
  go _ _ [] = []
genStrip _ = error "genStrip: the list length must be equal or longer than 3"
{-# INLINE genStrip #-}

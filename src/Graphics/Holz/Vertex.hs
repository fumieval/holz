{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Simple drawing operations
---------------------------------------------------------------------------
module Graphics.Holz.Vertex where
import Control.Lens
import Graphics.Holz
import Linear
import Control.Monad.IO.Class
import qualified Data.Vector.Storable as V

rectangle :: V4 Float -> V2 Float -> V2 Float -> (PrimitiveMode, [Vertex])
rectangle col (V2 x0 y0) (V2 x1 y1) = (TriangleStrip,
  [ Vertex (V3 x0 y0 0) (V2 0 0) (V3 0 0 1) col
  , Vertex (V3 x1 y0 0) (V2 1 0) (V3 0 0 1) col
  , Vertex (V3 x0 y1 0) (V2 0 1) (V3 0 0 1) col
  , Vertex (V3 x1 y1 0) (V2 1 1) (V3 0 0 1) col
  ])

-- | Make a translation matrix.
translate :: V3 Float -> M44 Float
translate v = identity & translation .~ v

-- | Draw vertices through the given model matrix.
draw :: MonadHolz m => M44 Float -> (PrimitiveMode, [Vertex]) -> m ()
draw m (prim, vs) = do
  buf <- registerVertex prim vs
  drawVertexPlain m buf
  releaseVertex buf

{-# LANGUAGE FlexibleContexts #-}
module Graphics.Parkett.Vertex where
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

translate :: V3 Float -> M44 Float
translate v = identity & translation .~ v

draw :: (MonadIO m, Given Window) => M44 Float -> (PrimitiveMode, [Vertex]) -> m ()
draw m (prim, vs) = do
  buf <- registerVertex prim vs
  drawVertexPlain m buf
  releaseVertex buf

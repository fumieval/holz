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
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.Holz
import Linear
import Control.Monad.IO.Class
import qualified Data.Vector.Storable as V

data Vertex = Vertex
    {-# UNPACK #-} !(V3 Float) -- x, y, z
    {-# UNPACK #-} !(V2 Float) -- u, v
    {-# UNPACK #-} !(V3 Float) -- normal
    {-# UNPACK #-} !(V4 Float) -- r, g, b, a

vertexAttributes :: IO ()
vertexAttributes = do
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride pos'
  glEnableVertexAttribArray 1

  glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE stride pos''
  glEnableVertexAttribArray 2

  glVertexAttribPointer 3 4 GL_FLOAT GL_FALSE stride pos'''
  glEnableVertexAttribArray 3
  where
    stride = fromIntegral $ sizeOf (undefined :: Vertex)
    pos' = nullPtr `plusPtr` sizeOf (0 :: V3 CFloat)
    pos'' = pos' `plusPtr` sizeOf (0 :: V2 CFloat)
    pos''' = pos'' `plusPtr` sizeOf (0 :: V3 CFloat)
{-# INLINE vertexAttributes #-}

registerVertex :: MonadIO m => PrimitiveMode -> [Vertex] -> m VertexBuffer
registerVertex mode = makeVertexBuffer vertexAttributes mode . V.fromList

align1 :: Int
align1 = sizeOf (undefined :: V3 Float)

align2 :: Int
align2 = align1 + sizeOf (undefined :: V2 Float)

align3 :: Int
align3 = align2 + sizeOf (undefined :: V3 Float)

instance Storable Vertex where
  sizeOf _ = sizeOf (undefined :: V3 Float)
      + sizeOf (undefined :: V2 Float)
      + sizeOf (undefined :: V3 Float)
      + sizeOf (undefined :: V4 Float)
  {-# INLINE sizeOf #-}
  alignment _ = 0
  {-# INLINE alignment #-}
  peek ptr = Vertex
    <$> peek ptr'
    <*> peek (ptr' `plusPtr` align1)
    <*> peek (ptr' `plusPtr` align2)
    <*> peek (ptr' `plusPtr` align3)
    where ptr' = castPtr ptr
  {-# INLINE peek #-}
  poke ptr (Vertex v t n c) = do
    poke ptr' v
    poke (ptr' `plusPtr` align1) t
    poke (ptr' `plusPtr` align2) n
    poke (ptr' `plusPtr` align3) c
    where ptr' = castPtr ptr
  {-# INLINE poke #-}

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

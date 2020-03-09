{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
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
module Graphics.Holz.Shader.Simple
  ( Vertex(..)
  , rectangle
  , translate
  , drawVertex
  , drawVertexPlain
  , makeDefaultShader
  , setOrthographic
  , setProjection
  , HasModel(..)
  , ModelProj(..)
  , Fragment(..)
  , HasSimpleShader
  , vertexShaderSource
  , fragmentShaderSource
  , Drawable(..)
  , draw
  -- * from Vertex
  , FromV3RGBAUV(..)
  , RGBA
  ) where

import Control.Lens
import Control.Monad.Reader
import Foreign
import GHC.Generics (Generic)
import Graphics.Holz.System
import Graphics.Holz.Shader
import Linear
import qualified Data.Vector.Storable as V
import qualified Text.RawString.QQ as QQ

data Vertex = Vertex
    { vPosition :: {-# UNPACK #-} !(V3 Float) -- x, y, z
    , vRGBA :: {-# UNPACK #-} !RGBA -- r, g, b, a
    , vUV :: {-# UNPACK #-} !(V2 Float) -- u, v
    } deriving (Show, Generic)
    deriving Storable via (StorableVertex Vertex)

class FromV3RGBAUV v where
  fromV3RGBAUV :: V3 Float -> RGBA -> V2 Float -> v

instance FromV3RGBAUV Vertex where
  fromV3RGBAUV = Vertex

rectangle :: RGBA -> V2 Float -> V2 Float -> (PrimitiveMode, V.Vector Vertex)
rectangle col (V2 x0 y0) (V2 x1 y1) = (TriangleStrip, V.fromList
  [ Vertex (V3 x0 y0 0) col (V2 0 0)
  , Vertex (V3 x1 y0 0) col (V2 1 0)
  , Vertex (V3 x0 y1 0) col (V2 0 1)
  , Vertex (V3 x1 y1 0) col (V2 1 1)
  ])

-- | Make a translation matrix.
translate :: V3 Float -> M44 Float
translate v = identity & translation .~ v

type HasSimpleShader r = (HasShader r, ShaderUniform r ~ ModelProj)

-- | Set the projection matrix.
setProjection :: (MonadHolz r m, HasSimpleShader r) => M44 Float -> m ()
setProjection = setUniform mpProjection

drawVertex :: (MonadHolz r m, HasShader r, HasSimpleShader r) => M44 Float -> Texture pixel -> VertexBuffer v -> m ()
drawVertex mat tex vb = do
  setUniform mpModel mat
  drawVertexBuffer tex vb

drawVertexPlain :: (MonadHolz r m, HasShader r, HasSimpleShader r) => M44 Float -> VertexBuffer v -> m ()
drawVertexPlain m = drawVertex m blankTexture
{-# INLINE drawVertexPlain #-}

makeDefaultShader :: MonadIO m => m (Shader ModelProj Vertex)
makeDefaultShader = makeShader vertexShaderSource fragmentShaderSource

-- | Set orthographic projection
setOrthographic :: (MonadHolz r m, HasWindow r, HasShader r, ShaderUniform r ~ ModelProj) => m ()
setOrthographic = do
  box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
  setViewport box
  setProjection $ ortho x0 x1 y1 y0 (-1) 1

data ModelProj h = ModelProj
  { mpProjection :: h (M44 Float)
  , mpModel :: h (M44 Float)
  } deriving Generic

class HasModel r where
  getModelVar :: r -> UniformVar (M44 Float)

instance HasModel (ModelProj UniformVar) where
  getModelVar = mpModel

type RGBA = V4 Float

data Fragment = Fragment
  { fTexUV :: V2 Float
  , fColor :: RGBA
  , fPos :: V4 Float
  } deriving Generic

vertexShaderSource :: VertexShaderSource ModelProj Vertex Fragment
vertexShaderSource = [QQ.r|
void main(void) {
  fPos = mpModel * vec4(vPosition, 1.0);
  gl_Position = mpProjection * fPos;
  fTexUV = vUV;
  fColor = vRGBA;
}
|]

fragmentShaderSource :: FragmentShaderSource ModelProj Fragment
fragmentShaderSource = [QQ.r|
uniform sampler2D tex;
out vec4 fragColor;
void main(void){
    fragColor = fColor * texture(tex, fTexUV);
}
|]

data Drawable = forall p v. Drawable !(Texture p) !(VertexBuffer v)

draw :: forall r m. (HasShader r, HasModel (ShaderUniform r UniformVar), MonadHolz r m) => M44 Float -> Drawable -> m ()
draw mat (Drawable tex buf) = do
  setUniform getModelVar mat
  drawVertexBuffer tex buf 
{-# INLINE draw #-}
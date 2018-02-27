{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Graphics.GL
import Graphics.Holz.System
import Linear
import Control.Monad.Free.Class
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

registerVertex :: (MonadHolz r m, HasShader r) => PrimitiveMode -> [Vertex] -> m VertexBuffer
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
draw :: (MonadHolz r m, HasShader r) => M44 Float -> (PrimitiveMode, [Vertex]) -> m ()
draw m (prim, vs) = do
  buf <- registerVertex prim vs
  drawVertexPlain m buf
  releaseVertex buf

data Shader = Shader
  { shaderProg :: {-# UNPACK #-} !GLuint
  , locationModel :: {-# UNPACK #-} !GLint
  , locationProjection :: {-# UNPACK #-} !GLint
  , locationDiffuse :: {-# UNPACK #-} !GLint
  }

class HasShader r where
  getShader :: r -> Shader

instance HasShader Shader where
  getShader = id

-- | Set the projection matrix.
setProjection :: (MonadHolz r m, HasShader r) => M44 Float -> m ()
setProjection proj = asks getShader >>= \w -> liftIO $ with proj
  $ \ptr -> glUniformMatrix4fv (locationProjection w) 1 1 $ castPtr ptr

-- | Set a diffuse color.
setDiffuse :: (MonadHolz r m, HasShader r) => V4 Float -> m ()
setDiffuse col = asks getShader >>= \w -> liftIO $ with col $ \ptr -> glUniform4fv (locationDiffuse w) 1 (castPtr ptr)

drawVertex :: (MonadHolz r m, HasShader r) => M44 Float -> Texture -> VertexBuffer -> m ()
drawVertex mat tex vb = asks getShader >>= \w -> liftIO $ do
  with mat $ \p -> glUniformMatrix4fv (locationModel w) 1 1 (castPtr p)
  drawVertexBuffer tex vb

drawVertexPlain :: (MonadHolz r m, HasShader r) => M44 Float -> VertexBuffer -> m ()
drawVertexPlain m = drawVertex m blankTexture
{-# INLINE drawVertexPlain #-}

data WithShader r = WithShader !Shader !r

instance HasWindow r => HasWindow (WithShader r) where
  getWindow (WithShader _ w) = getWindow w

instance HasShader (WithShader r) where
  getShader (WithShader s _) = s

type ShaderT r = ReaderT (WithShader r)

withDefaultShader :: (MonadHolz r m) => ShaderT r m a -> m a
withDefaultShader m = do
  sh <- liftIO makeShader
  runShaderT sh m

runShaderT :: (MonadHolz r m) => Shader -> ShaderT r m a -> m a
runShaderT sh m = ask >>= runReaderT m . WithShader sh

-- | Set orthographic projection
setOrthographic :: (MonadHolz r m, HasWindow r, HasShader r) => m ()
setOrthographic = do
  box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
  setViewport box
  setProjection $ ortho x0 x1 y1 y0 (-1) 1

makeShader :: IO Shader
makeShader = do
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  compileShader vertexShaderSource vertexShader
  compileShader fragmentShaderSource fragmentShader

  shaderProg <- glCreateProgram
  glAttachShader shaderProg vertexShader
  glAttachShader shaderProg fragmentShader

  withCString "in_Position" $ glBindAttribLocation shaderProg 0
  withCString "in_UV" $ glBindAttribLocation shaderProg 1
  withCString "in_Normal" $ glBindAttribLocation shaderProg 2
  withCString "in_Color" $ glBindAttribLocation shaderProg 3

  glLinkProgram shaderProg
  glUseProgram shaderProg

  linked <- overPtr (glGetProgramiv shaderProg GL_LINK_STATUS)
  when (linked == GL_FALSE) $ do
    maxLength <- overPtr (glGetProgramiv shaderProg GL_INFO_LOG_LENGTH)
    allocaArray (fromIntegral maxLength) $ \ptr -> do
      glGetProgramInfoLog shaderProg maxLength nullPtr ptr
      peekCString ptr >>= putStrLn

  locationModel <- getUniform shaderProg "model"
  locationProjection <- getUniform shaderProg "projection"
  locationDiffuse <- getUniform shaderProg "diffuse"

  with (V4 1 1 1 1 :: V4 Float) $ \ptr -> do
    glUniform4fv locationDiffuse 1 (castPtr ptr)

  return Shader{..}

vertexShaderSource :: String
vertexShaderSource = "#version 330\n\
  \uniform mat4 projection; \
  \uniform mat4 model; \
  \in vec3 in_Position; \
  \in vec2 in_UV; \
  \in vec3 in_Normal; \
  \in vec4 in_Color; \
  \out vec2 texUV; \
  \out vec3 normal; \
  \out vec4 viewPos; \
  \out vec4 color; \
  \void main(void) { \
  \  viewPos = model * vec4(in_Position, 1.0); \
  \  gl_Position = projection * viewPos; \
  \  texUV = in_UV; \
  \  normal = in_Normal;\
  \  color = in_Color;\
  \}"

fragmentShaderSource :: String
fragmentShaderSource = "#version 330\n\
  \out vec4 fragColor; \
  \in vec2 texUV; \
  \in vec3 normal; \
  \in vec4 viewPos; \
  \in vec4 color; \
  \uniform sampler2D tex; \
  \uniform vec4 diffuse; \
  \void main(void){ \
  \  fragColor = texture(tex, texUV) * color * diffuse; \
  \}"

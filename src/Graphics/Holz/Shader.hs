{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Graphics.Holz.Shader
  ( -- * Vertex
   VertexBuffer
  , PrimitiveMode(..)
  , registerVertex
  , releaseVertex
  , withVertex
  , drawVertexBuffer
  -- * Texture
  , Texture
  , blankTexture
  , registerTextures
  , registerTexture
  , releaseTexture
  , withTexture
  , Shader(..)
  , HasShader(..)
  , ShaderProg
  -- * Uniform
  , UniformVar
  , setUniform
  -- * Compilation
  , makeShader
  , VertexShaderSource(..)
  , FragmentShaderSource(..)
  -- * Variable
  , ShaderField(..)
  , GUniformVars
  , GShaderVars
  , StorableVertex(..)
  )
  where
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Proxy
import Data.String
import Data.Functor.Identity
import Foreign.Storable
import Foreign.Ptr
import Foreign (with)
import GHC.Generics
import GHC.TypeLits
import Graphics.GL
import Graphics.Holz.System (MonadHolz)
import Linear
import Foreign.C.String
import qualified Data.Vector.Storable as V
import Codec.Picture
import Data.Word (Word8)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import System.IO.Unsafe
import UnliftIO (MonadUnliftIO, bracket)

newtype UniformVar a = UniformVar GLint

class Storable a => ShaderField a where
  typeGL :: Proxy a -> BB.Builder
  elemGL :: Proxy a -> GLint
  elemTypeGL :: Proxy a -> GLuint
  setUniformVar :: UniformVar a -> a -> IO ()

deriving instance ShaderField a => ShaderField (Identity a)

instance ShaderField Float where
  typeGL _ = "float"
  elemGL _ = 1
  elemTypeGL _ = GL_FLOAT
  setUniformVar (UniformVar loc) v = with v $ glUniform1fv loc 1 . castPtr

instance ShaderField (V2 Float) where
  typeGL _ = "vec2"
  elemGL _ = 2
  elemTypeGL _ = GL_FLOAT
  setUniformVar (UniformVar loc) v = with v $ glUniform2fv loc 1 . castPtr

instance ShaderField (V3 Float) where
  typeGL _ = "vec3"
  elemGL _ = 3
  elemTypeGL _ = GL_FLOAT
  setUniformVar (UniformVar loc) v = with v $ glUniform3fv loc 1 . castPtr

instance ShaderField (V4 Float) where
  typeGL _ = "vec4"
  elemGL _ = 4
  elemTypeGL _ = GL_FLOAT
  setUniformVar (UniformVar loc) v = with v $ glUniform4fv loc 1 . castPtr

instance ShaderField (V4 (V4 Float)) where
  typeGL _ = "mat4"
  elemGL _ = 16
  elemTypeGL _ = GL_FLOAT
  setUniformVar (UniformVar loc) v = with v $ glUniformMatrix4fv loc 1 1 . castPtr

fieldDecs :: forall a. (Generic a, GShaderVars (Rep a)) => BB.Builder -> BB.Builder
fieldDecs p = gfieldDecs @ (Rep a) p

class GShaderVars (f :: * -> *) where
  gfieldDecs :: BB.Builder -> BB.Builder

instance GShaderVars U1 where
  gfieldDecs = const mempty

instance (KnownSymbol name, m ~ 'MetaSel ('Just name) u s l, ShaderField a) => GShaderVars (S1 m (K1 i a)) where
  gfieldDecs prefix = prefix <+> typeGL (Proxy @ a) <+> fromString (symbolVal (Proxy @ name)) <> ";" where
    x <+> y = x <> " " <> y

instance (GShaderVars f) => GShaderVars (D1 m f) where
  gfieldDecs = gfieldDecs @ f

instance (GShaderVars f) => GShaderVars (C1 m f) where
  gfieldDecs = gfieldDecs @ f

instance (GShaderVars f, GShaderVars g) => GShaderVars (f :*: g) where
  gfieldDecs prefix = gfieldDecs @f prefix <> gfieldDecs @g prefix

newtype ShaderProg v = ShaderProg GLuint

newtype StorableVertex a = StorableVertex { unStorableVertex :: a }

instance (Generic v, GVertex (Rep v)) => Storable (StorableVertex v) where
  sizeOf _ = sizeVertex @ v
  alignment _ = 1
  peek = error "peek is not implemented yet"
  poke ptr = pokeVertex (castPtr ptr) . unStorableVertex

vertexAttributes :: forall v. (Generic v, GVertex (Rep v)) => IO ()
vertexAttributes = void $ gvertexAttributes @ (Rep v) (gsizeVertex @ (Rep v)) 0 nullPtr

bindAttribLocation :: forall v. (Generic v, GVertex (Rep v)) => ShaderProg v -> IO ()
bindAttribLocation prog = void $ gbindAttribLocation @ (Rep v) prog 0

pokeVertex :: forall v. (Generic v, GVertex (Rep v)) => Ptr v -> v -> IO ()
pokeVertex ptr x = void $ gpokeVertex (from x) ptr

sizeVertex :: forall v. (Generic v, GVertex (Rep v)) => Int
sizeVertex = fromIntegral $ gsizeVertex @ (Rep v)

class GVertex (f :: * -> *) where
  gvertexAttributes :: GLint -> GLuint -> Ptr () -> IO (GLuint, Ptr ())
  gbindAttribLocation :: ShaderProg v -> GLuint -> IO GLuint
  gpokeVertex :: f x -> Ptr a -> IO (Ptr a)
  gsizeVertex :: GLint

instance GVertex U1 where
  gvertexAttributes _ i p = pure (i, p)
  gbindAttribLocation _ v = pure v
  gpokeVertex _ = pure
  gsizeVertex = 0

instance GVertex f => GVertex (D1 m f) where
  gvertexAttributes = gvertexAttributes @ f
  gbindAttribLocation = gbindAttribLocation @ f
  gpokeVertex (M1 x) = gpokeVertex x
  gsizeVertex = gsizeVertex @ f

instance GVertex f => GVertex (C1 m f) where
  gvertexAttributes = gvertexAttributes @ f
  gbindAttribLocation = gbindAttribLocation @ f
  gpokeVertex (M1 x) = gpokeVertex x
  gsizeVertex = gsizeVertex @ f

instance (KnownSymbol name, m ~ 'MetaSel ('Just name) u s l, ShaderField a) => GVertex (S1 m (K1 i a)) where
  gvertexAttributes stride i ptr = do
    let p = Proxy @ a
    glVertexAttribPointer i (elemGL p) (elemTypeGL p) GL_FALSE stride ptr
    glEnableVertexAttribArray i
    return (i + 1, ptr `plusPtr` sizeOf (undefined :: a))
  gbindAttribLocation (ShaderProg shader) i = do
    withCString (symbolVal (Proxy @ name)) $ glBindAttribLocation shader i
    return $! i + 1
  gpokeVertex (M1 (K1 a)) ptr = ptr `plusPtr` sizeOf a <$ poke (castPtr ptr) a
  gsizeVertex = fromIntegral $ sizeOf (undefined :: a)

instance (GVertex f, GVertex g) => GVertex (f :*: g) where
  gvertexAttributes stride i ptr = do
    (j, ptr') <- gvertexAttributes @f stride i ptr
    gvertexAttributes @ g stride j ptr'
  gbindAttribLocation sh = gbindAttribLocation @f sh >=> gbindAttribLocation @g sh
  gpokeVertex (f :*: g) = gpokeVertex f >=> gpokeVertex g
  gsizeVertex = gsizeVertex @ f + gsizeVertex @ g

-- | Send vertices to the graphics driver.
registerVertex :: forall m v. (Generic v, GVertex (Rep v), Storable v, MonadIO m)
  => PrimitiveMode -> V.Vector v -> m (VertexBuffer v)
registerVertex mode va = liftIO $ do
  vao <- overPtr $ glGenVertexArrays 1
  glBindVertexArray vao
  vbo <- overPtr $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  vertexAttributes @ v
  let siz = fromIntegral $ V.length va * sizeOf (undefined :: v)
  V.unsafeWith va $ \v -> glBufferData GL_ARRAY_BUFFER siz (castPtr v) GL_STATIC_DRAW
  return $! VertexBuffer vao vbo (convPrimitiveMode mode) (fromIntegral $ V.length va)

-- | Release a 'VertexBuffer'. The 'VertexBuffer' can't be used after this.
releaseVertex :: MonadIO m => VertexBuffer v -> m ()
releaseVertex (VertexBuffer vao vbo _ _) = liftIO $ do
  with vao $ glDeleteVertexArrays 1
  with vbo $ glDeleteBuffers 1

withVertex :: (Generic v, GVertex (Rep v), Storable v, MonadUnliftIO m) => PrimitiveMode -> V.Vector v -> (VertexBuffer v -> m r) -> m r
withVertex m v = bracket (registerVertex m v) releaseVertex

-- | Draw 'VertexBuffer' using the w 'Texture' and a model matrix.
drawVertexBuffer :: MonadIO m => Texture -> VertexBuffer v -> m ()
drawVertexBuffer (Texture tex) (VertexBuffer vao vbo m n) = liftIO $ do
  glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  glDrawArrays m 0 n

-- | 'PrimitiveMode' describes how vertices will be drawn.
data PrimitiveMode = Triangles | TriangleFan | TriangleStrip | LineStrip | LineLoop

convPrimitiveMode :: PrimitiveMode -> GLenum
convPrimitiveMode Triangles = GL_TRIANGLES
convPrimitiveMode LineStrip = GL_LINE_STRIP
convPrimitiveMode TriangleFan = GL_TRIANGLE_FAN
convPrimitiveMode TriangleStrip = GL_TRIANGLE_STRIP
convPrimitiveMode LineLoop = GL_LINE_LOOP
{-# INLINE convPrimitiveMode #-}

data VertexBuffer v = VertexBuffer !GLuint !GLuint !GLenum !GLsizei

instance Eq (VertexBuffer v) where
  VertexBuffer i _ _ _ == VertexBuffer j _ _ _ = i == j

instance Ord (VertexBuffer v) where
  VertexBuffer i _ _ _ `compare` VertexBuffer j _ _ _ = compare i j

newtype Texture = Texture GLuint deriving (Eq, Ord)

-- | Send an image into the graphics driver.
registerTexture :: MonadIO m => Image PixelRGBA8 -> m Texture
registerTexture img@(Image w h _) = registerTextures (V2 w h) [(V2 0 0, img)]

-- | A blank texture.
blankTexture :: Texture
blankTexture = unsafePerformIO $ do
  tex <- overPtr (glGenTextures 1)
  glBindTexture GL_TEXTURE_2D tex
  with (pure 255 :: V3 Word8) $ glTexImage2D GL_TEXTURE_2D 0 GL_SRGB8 1 1 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr
  return $ Texture tex
{-# NOINLINE blankTexture #-}

-- | Send a set of images into the graphics driver.
registerTextures :: MonadIO m => V2 Int -> [(V2 Int, Image PixelRGBA8)] -> m Texture
registerTextures (V2 sw sh) imgs = liftIO $ do
  tex <- overPtr (glGenTextures 1)
  glBindTexture GL_TEXTURE_2D tex
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
  glPixelStorei GL_UNPACK_ALIGNMENT 4
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_LSB_FIRST 0
  glPixelStorei GL_UNPACK_ROW_LENGTH 0
  glPixelStorei GL_UNPACK_SKIP_IMAGES 0
  glPixelStorei GL_UNPACK_SKIP_PIXELS 0
  glPixelStorei GL_UNPACK_SKIP_ROWS 0
  glPixelStorei GL_UNPACK_SWAP_BYTES 0
  let level = floor $ logBase (2 :: Float) $ fromIntegral (max sw sh)

  glTexStorage2D GL_TEXTURE_2D level GL_RGBA8 (fromIntegral sw) (fromIntegral sh)

  -- when gl_EXT_texture_filter_anisotropic
  --  $ glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT 8

  forM_ imgs $ \(V2 x y, Image w h vec) -> V.unsafeWith vec
    $ glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE
    . castPtr

  glGenerateMipmap GL_TEXTURE_2D

  return $ Texture tex

releaseTexture :: MonadIO m => Texture -> m ()
releaseTexture (Texture i) = liftIO $ with i $ glDeleteTextures 1

withTexture :: MonadUnliftIO m => Image PixelRGBA8 -> (Texture -> m a) -> m a
withTexture img = bracket (registerTexture img) releaseTexture

compileShader :: BB.Builder -> GLuint -> IO ()
compileShader src shader = do
  B.useAsCString (BL.toStrict $ BB.toLazyByteString $ "#version 330\n" <> src) $ \ptr -> withArray [ptr]
    $ \srcs -> glShaderSource shader 1 srcs nullPtr
  glCompileShader shader

  l <- overPtr $ glGetShaderiv shader GL_INFO_LOG_LENGTH
  allocaArray (fromIntegral l) $ \ptr -> do
    glGetShaderInfoLog shader l nullPtr ptr
    peekCString ptr >>= putStrLn


overPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
overPtr f = alloca $ \p -> f p >> peek p
{-# INLINE overPtr #-}

setUniform
  :: (MonadHolz r m, HasShader r, ShaderField a)
  => (ShaderUniform r UniformVar -> UniformVar a)
  -> a -> m ()
setUniform sel x = do
  v <- asks $ sel . shaderUniform . getShader
  liftIO $ setUniformVar v x

class GUniformVars f where
  guniformVars :: ShaderProg a -> IO (f x)

instance (GUniformVars f, GUniformVars g) => GUniformVars (f :*: g) where
  guniformVars prog = (:*:) <$> guniformVars prog <*> guniformVars prog

instance (GUniformVars f) => GUniformVars (D1 m f) where
  guniformVars prog = M1 <$> guniformVars prog

instance (GUniformVars f) => GUniformVars (C1 m f) where
  guniformVars prog = M1 <$> guniformVars prog

instance (KnownSymbol name, m ~ 'MetaSel ('Just name) u s l) => GUniformVars (S1 m (K1 i (UniformVar a))) where
  guniformVars (ShaderProg prog) = M1 . K1 . UniformVar <$> withCString (symbolVal (Proxy @ name)) (glGetUniformLocation prog)

--- TODO genralise this

data Shader u v = Shader
  { shaderProgram :: {-# UNPACK #-} !(ShaderProg v)
  , shaderUniform :: u UniformVar
  }

class HasShader r where
  type ShaderUniform r :: (* -> *) -> *
  type ShaderVertex r :: *
  getShader :: r -> Shader (ShaderUniform r) (ShaderVertex r)

instance HasShader (Shader u v) where
  type ShaderVertex (Shader u v) = v
  type ShaderUniform (Shader u v) = u
  getShader = id

newtype VertexShaderSource (uniform :: (* -> *) -> *) vert frag = VertexShaderSource BB.Builder deriving IsString
newtype FragmentShaderSource (uniform :: (* -> *) -> *) frag = FragmentShaderSource BB.Builder deriving IsString

makeShader :: forall uniform vert frag m.
  ( Generic vert
  , Generic frag
  , Generic (uniform Identity)
  , Generic (uniform UniformVar)
  , GVertex (Rep vert)
  , GShaderVars (Rep (uniform Identity))
  , GUniformVars (Rep (uniform UniformVar))
  , GShaderVars (Rep vert)
  , GShaderVars (Rep frag)
  , MonadIO m)
  => VertexShaderSource uniform vert frag
  -> FragmentShaderSource uniform frag
  -> m (Shader uniform vert)
makeShader (VertexShaderSource vsBody) (FragmentShaderSource fsBody) = liftIO $ do
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER

  let uni = fieldDecs @ (uniform Identity) "uniform"
  let vSrc = uni <> fieldDecs @ vert "in" <> fieldDecs @ frag "out" <> vsBody
  let fSrc = uni <> fieldDecs @ frag "in" <> fsBody
  compileShader vSrc vertexShader
  compileShader fSrc fragmentShader

  shaderProg <- glCreateProgram
  glAttachShader shaderProg vertexShader
  glAttachShader shaderProg fragmentShader

  let shaderProgram = ShaderProg shaderProg

  bindAttribLocation shaderProgram

  glLinkProgram shaderProg
  glUseProgram shaderProg

  linked <- overPtr (glGetProgramiv shaderProg GL_LINK_STATUS)
  when (linked == GL_FALSE) $ do
    maxLength <- overPtr (glGetProgramiv shaderProg GL_INFO_LOG_LENGTH)
    allocaArray (fromIntegral maxLength) $ \ptr -> do
      glGetProgramInfoLog shaderProg maxLength nullPtr ptr
      peekCString ptr >>= putStrLn

  shaderUniform <- to <$> guniformVars shaderProgram

  return Shader{..}

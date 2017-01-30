{-# LANGUAGE FlexibleContexts, Rank2Types, LambdaCase, GeneralizedNewtypeDeriving, ConstraintKinds, ScopedTypeVariables #-}
---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The main API of holz
---------------------------------------------------------------------------
module Graphics.Holz.System (withHolz
  -- * Window operation
  , Window
  , openWindow
  , WindowMode(..)
  , Box.Box(..)
  , windowShouldClose
  , withFrame
  , setTitle
  , clearColor
  -- * Monad
  , MonadHolz
  , HolzT(..)
  , runHolzT
  , retract
  -- * Textures
  , Texture
  , registerTexture
  , registerTextures
  , releaseTexture
  , blankTexture
  -- * Vertices
  , PrimitiveMode(..)
  , VertexBuffer
  , makeVertexBuffer
  , releaseVertex
  -- * Drawing region
  , setViewport
  , getBoundingBox
  , setBoundingBox
  -- * Rendering
  , setProjection
  , setOrthographic
  , drawVertex
  , drawVertexPlain
  , setDiffuse
  -- * Input (callback)
  , linkKeyboard
  , linkMouseButton
  , linkMouseCursor
  , linkMouseScroll
  -- * Input (poll)
  , keyPress
  , getCursorPos
  , mousePress
  , typedString
  , typedKeys
  -- * Misc
  , takeScreenshot
  , enableCursor
  , disableCursor
  , hideCursor
  , overPtr
  ) where

import Codec.Picture
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Iter
import Data.Bits
import Data.BoundingBox as Box
import Data.IORef
import Foreign
import Foreign.C (CFloat)
import Foreign.C.String
import Graphics.GL
import Linear
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified GHC.IO.Encoding as Encoding
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic
import Graphics.Holz.Input
import Data.Reflection
import System.Mem.Weak
import System.IO.Unsafe
import Control.Applicative

data Window = Window
  { refRegion :: {-# UNPACK #-} !(IORef (Box V2 Float))
  , theWindow :: {-# UNPACK #-} !GLFW.Window
  , theProgram :: {-# UNPACK #-} !GLuint
  , locationModel :: {-# UNPACK #-} !GLint
  , locationProjection :: {-# UNPACK #-} !GLint
  , locationDiffuse :: {-# UNPACK #-} !GLint
  , locationSpecular :: {-# UNPACK #-} !GLint
  , keyboardHandlers :: {-# UNPACK #-} !(IORef (Chatter Key -> IO ()))
  , mouseButtonHandlers :: {-# UNPACK #-} !(IORef (Chatter Int -> IO ()))
  , mouseCursorHandlers :: {-# UNPACK #-} !(IORef (V2 Float -> IO ()))
  , mouseScrollHandlers :: {-# UNPACK #-} !(IORef (V2 Float -> IO ()))
  , keysTyped :: !(IORef ([Key], [Key]))
  , charactersTyped :: !(IORef ([Char], [Char]))
  }

type MonadHolz m = (MonadIO m, MonadReader Window m)

newtype HolzT m a = HolzT { unHolzT :: IterT (ReaderT Window m) a }
  deriving (Functor, Applicative, Alternative
    , Monad, MonadPlus, MonadReader Window, MonadIO, MonadFree Identity)

instance MonadTrans HolzT where
  lift m = HolzT $ lift $ lift m

-- | Set a handler for key events.
linkKeyboard :: MonadHolz m => (Chatter Key -> IO ()) -> m ()
linkKeyboard f = ask >>= \s -> liftIO $ modifyIORef (keyboardHandlers s) (liftA2 (>>) f)

-- | Set a handler for mouse button events.
linkMouseButton :: MonadHolz m => (Chatter Int -> IO ()) -> m ()
linkMouseButton f = ask >>= \s -> liftIO $ modifyIORef (mouseButtonHandlers s) (liftA2 (>>) f)

-- | Set a handler for cursor movement.
linkMouseCursor :: MonadHolz m => (V2 Float -> IO ()) -> m ()
linkMouseCursor f = ask >>= \s -> liftIO $ modifyIORef (mouseCursorHandlers s) (liftA2 (>>) f)

-- | Set a handler for scroll events.
linkMouseScroll :: MonadHolz m => (V2 Float -> IO ()) -> m ()
linkMouseScroll f = ask >>= \s -> liftIO $ modifyIORef (mouseScrollHandlers s) (liftA2 (>>) f)

-- | 'PrimitiveMode' describes how vertices will be drawn.
data PrimitiveMode = Triangles | TriangleFan | TriangleStrip | LineStrip | LineLoop

data WindowMode = FullScreen | Resizable | Windowed deriving (Show, Eq, Ord)

-- | Returns 'True' when the window should be closed.
windowShouldClose :: MonadHolz m => m Bool
windowShouldClose = ask >>= \s -> liftIO $ GLFW.windowShouldClose (theWindow s)

-- | Run actions for a window.
withFrame :: MonadHolz m => m a -> m a
withFrame m = do
  win <- ask
  liftIO $ GLFW.makeContextCurrent $ Just $ theWindow win
  liftIO $ glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  a <- m
  liftIO $ modifyIORef' (charactersTyped win) $ \(_, buf) -> (reverse buf, [])
  liftIO $ modifyIORef' (keysTyped win) $ \(_, buf) -> (reverse buf, [])
  liftIO $ GLFW.swapBuffers $ theWindow win
  liftIO GLFW.pollEvents
  return a

-- | Run an 'IterT' computation on a 'Window'. It returns 'Nothing' if the window is closed.
-- The resulting 'IterT' can be composed with ('<|>') to update multiple windows in parallel.
runHolzT :: MonadIO m => Window -> HolzT m a -> IterT m (Maybe a)
runHolzT win = flip runReaderT win . go . unHolzT where
  go m = join $ withFrame $ windowShouldClose >>= \case
    False -> do
      w <- ask
      setOrthographic
      either (return . Just) (delay . go)
        <$> lift (lift $ runIterT m `runReaderT` w)
    True -> return (return Nothing)

-- | Set orthographic projection
setOrthographic :: MonadHolz m => m ()
setOrthographic = do
    box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
    setViewport $ fmap round box
    setProjection $ ortho x0 x1 y1 y0 (-1) 1

-- | Open a window.
openWindow :: WindowMode -> Box V2 Float -> IO Window
openWindow windowmode bbox@(Box (V2 x0 y0) (V2 x1 y1)) = do
  let ww = floor $ x1 - x0
      wh = floor $ y1 - y0

  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'Resizable (windowmode == Resizable)

  mon <- if windowmode == FullScreen then GLFW.getPrimaryMonitor else return Nothing
  win <- GLFW.createWindow ww wh "" mon Nothing
    >>= maybe (fail "Failed to create a window") return

  GLFW.makeContextCurrent $ Just win
  -- GLFW.swapInterval 1

  (fw, fh) <- GLFW.getFramebufferSize win

  prog <- initializeGL

  rbox <- newIORef $ bbox & Box.size zero .~ fmap fromIntegral (V2 fw fh)

  locM <- getUniform prog "model"
  locP <- getUniform prog "projection"
  locD <- getUniform prog "diffuse"
  locS <- getUniform prog "specular"
  hk <- newIORef (const (return ()))
  hb <- newIORef (const (return ()))
  hc <- newIORef (const (return ()))
  hs <- newIORef (const (return ()))
  refKeys <- newIORef ([], [])
  refChars <- newIORef ([], [])

  with (V4 1 1 1 1 :: V4 Float) $ \ptr -> do
      glUniform4fv locD 1 (castPtr ptr)
      glUniform4fv locS 1 (castPtr ptr)

  GLFW.setFramebufferSizeCallback win $ Just
    $ \_ w h -> do
      GLFW.makeContextCurrent $ Just win
      modifyIORef rbox $ Box.size zero .~ fmap fromIntegral (V2 w h)
      glViewport 0 0 (fromIntegral w) (fromIntegral h)

  GLFW.setKeyCallback win $ Just $ keyCallback refKeys hk
  GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback hb
  GLFW.setCursorPosCallback win $ Just $ cursorPosCallback hc
  GLFW.setScrollCallback win $ Just $ scrollCallback hs
  GLFW.setCharCallback win $ Just $ \_ ch -> modifyIORef' refChars $ \(str, buf) -> (str, ch : buf)

  return $ Window rbox win prog locM locP locD locS hk hb hc hs refKeys refChars

-- | Close a window.
closeWindow :: Window -> IO ()
closeWindow = GLFW.destroyWindow . theWindow

-- | Any API except 'withHolz' has to be used inside 'withHolz'.
withHolz :: IO a -> IO a
withHolz m = do
  -- Encoding.setForeignEncoding Encoding.utf8

  GLFW.init >>= \r -> case r of
    False -> fail "Failed to initialize"
    True -> return ()

  a <- m

  GLFW.terminate

  return a

compileShader :: String -> GLuint -> IO ()
compileShader src shader = do
  withCString src $ \ptr -> withArray [ptr]
    $ \srcs -> glShaderSource shader 1 srcs nullPtr
  glCompileShader shader
  l <- overPtr $ glGetShaderiv shader GL_INFO_LOG_LENGTH
  allocaArray (fromIntegral l) $ \ptr -> do
    glGetShaderInfoLog shader l nullPtr ptr
    peekCString ptr >>= putStrLn

initializeGL :: IO GLuint
initializeGL = do
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

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glClearColor 0 0 0 1
  glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
  glDepthFunc GL_LEQUAL
  glDepthMask GL_TRUE
  glDisable GL_CULL_FACE
  glEnable GL_DEPTH_TEST
  glEnable GL_LINE_SMOOTH

  return shaderProg

getUniform :: GLuint -> String -> IO GLint
getUniform prog str = withCString str $ glGetUniformLocation prog

convPrimitiveMode :: PrimitiveMode -> GLenum
convPrimitiveMode Triangles = GL_TRIANGLES
convPrimitiveMode LineStrip = GL_LINE_STRIP
convPrimitiveMode TriangleFan = GL_TRIANGLE_FAN
convPrimitiveMode TriangleStrip = GL_TRIANGLE_STRIP
convPrimitiveMode LineLoop = GL_LINE_LOOP
{-# INLINE convPrimitiveMode #-}

data VertexBuffer = VertexBuffer !GLuint !GLuint !GLenum !GLsizei

instance Eq VertexBuffer where
  VertexBuffer i _ _ _ == VertexBuffer j _ _ _ = i == j

instance Ord VertexBuffer where
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
  glPixelStorei GL_UNPACK_ALIGNMENT 4
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_LSB_FIRST 0
  glPixelStorei GL_UNPACK_ROW_LENGTH 0
  glPixelStorei GL_UNPACK_SKIP_IMAGES 0
  glPixelStorei GL_UNPACK_SKIP_PIXELS 0
  glPixelStorei GL_UNPACK_SKIP_ROWS 0
  glPixelStorei GL_UNPACK_SWAP_BYTES 0
  let level = floor $ logBase (2 :: Float) $ fromIntegral (max sw sh)

  glTexStorage2D GL_TEXTURE_2D level GL_SRGB8_ALPHA8 (fromIntegral sw) (fromIntegral sh)

  -- when gl_EXT_texture_filter_anisotropic
  --  $ glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT 8

  forM_ imgs $ \(V2 x y, Image w h vec) -> V.unsafeWith vec
    $ glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE
    . castPtr

  glGenerateMipmap GL_TEXTURE_2D

  let t = Texture tex
  addFinalizer t $ with tex $ glDeleteTextures 1
  return t

releaseTexture :: Texture -> IO ()
releaseTexture (Texture i) = with i $ glDeleteTextures 1

-- | Send vertices to the graphics driver.
makeVertexBuffer :: forall m v. (Storable v, MonadIO m)
  => IO () -> PrimitiveMode -> V.Vector v -> m VertexBuffer
makeVertexBuffer conf mode va = liftIO $ do
  vao <- overPtr $ glGenVertexArrays 1
  glBindVertexArray vao
  vbo <- overPtr $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  conf
  let siz = fromIntegral $ V.length va * sizeOf (undefined :: v)
  V.unsafeWith va $ \v -> glBufferData GL_ARRAY_BUFFER siz (castPtr v) GL_STATIC_DRAW
  let vb = VertexBuffer vao vbo (convPrimitiveMode mode) (fromIntegral $ V.length va)
  -- addFinalizer vb $ do
  --  with vao $ glDeleteVertexArrays 1
  --  with vbo $ glDeleteBuffers 1
  return vb

-- | Release a 'VertexBuffer'. The 'VertexBuffer' can't be used after this.
releaseVertex :: MonadIO m => VertexBuffer -> m ()
releaseVertex (VertexBuffer vao vbo _ _) = liftIO $ do
  with vao $ glDeleteVertexArrays 1
  with vbo $ glDeleteBuffers 1

-- | Set the projection matrix.
setProjection :: MonadHolz m => M44 Float -> m ()
setProjection proj =ask >>= \w ->  liftIO $ with proj
  $ \ptr -> glUniformMatrix4fv (locationProjection w) 1 1 $ castPtr ptr

-- | Set a viewport.
setViewport :: MonadHolz m => Box V2 Int -> m ()
setViewport (Box (V2 x0 y0) (V2 x1 y1)) = liftIO $ glViewport
  (fromIntegral x0)
  (fromIntegral y0)
  (fromIntegral $ x1 - x0)
  (fromIntegral $ y1 - y0)

-- | Set a diffuse color.
setDiffuse :: MonadHolz m => V4 Float -> m ()
setDiffuse col = ask >>= \w -> liftIO $ with col $ \ptr -> glUniform4fv (locationDiffuse w) 1 (castPtr ptr)

drawVertexPlain :: MonadHolz m => M44 Float -> VertexBuffer -> m ()
drawVertexPlain m = drawVertex m blankTexture
{-# INLINE drawVertexPlain #-}

-- | Draw 'VertexBuffer' using the w 'Texture' and a model matrix.
drawVertex :: MonadHolz m => M44 Float -> Texture -> VertexBuffer -> m ()
drawVertex mat (Texture tex) (VertexBuffer vao vbo m n) = ask >>= \w -> liftIO $ do
  with mat $ \p -> glUniformMatrix4fv (locationModel w) 1 1 (castPtr p)
  glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  glDrawArrays m 0 n

-- | Hide the system cursor.
hideCursor :: MonadHolz m => m ()
hideCursor = ask >>= \w -> liftIO $ GLFW.setCursorInputMode (theWindow w) GLFW.CursorInputMode'Hidden

-- | Hide and pin the cursor against the program.
disableCursor :: MonadHolz m => m ()
disableCursor = ask >>= \w -> liftIO $ GLFW.setCursorInputMode (theWindow w) GLFW.CursorInputMode'Disabled

-- | Re-enable the cursor.
enableCursor :: MonadHolz m => m ()
enableCursor = ask >>= \w -> liftIO $ GLFW.setCursorInputMode (theWindow w) GLFW.CursorInputMode'Normal

-- | Set the background color.
clearColor :: MonadHolz m => V4 Float -> m ()
clearColor (V4 r g b a) = liftIO $ glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- | Set the size of the window and the resolution.
setBoundingBox :: MonadHolz m => Box V2 Float -> m ()
setBoundingBox box@(Box (V2 x0 y0) (V2 x1 y1)) = do
  w <- ask
  liftIO $ GLFW.setWindowSize (theWindow w) (floor (x1 - x0)) (floor (y1 - y0))
  liftIO $ writeIORef (refRegion w) box

getBoundingBox :: MonadHolz m => m (Box V2 Float)
getBoundingBox = ask >>= \w -> liftIO $ readIORef (refRegion w)

-- | Take a screenshot of the window.
takeScreenshot :: MonadHolz m => m (Image PixelRGBA8)
takeScreenshot = ask >>= \w -> liftIO $ do
  V2 w h <- fmap floor <$> view (Box.size zero) <$> readIORef (refRegion w)
  mv <- MV.unsafeNew $ w * h * 4 :: IO (MV.IOVector Word8)
  mv' <- MV.unsafeNew $ w * h * 4
  glReadBuffer GL_FRONT
  MV.unsafeWith mv $ \ptr -> do
    glReadPixels 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE (castPtr ptr)
    MV.unsafeWith mv' $ \dst -> forM_ [0..h-1] $ \y -> copyBytes (plusPtr dst $ y * w * 4) (plusPtr ptr $ (h - y - 1) * w * 4) (4 * w)
  Image w h <$> V.unsafeFreeze mv'

-- | Set the window title.
setTitle :: MonadHolz m => String -> m ()
setTitle str = ask >>= \w -> liftIO $ GLFW.setWindowTitle (theWindow w) str

keyCallback :: IORef ([Key], [Key]) -> IORef (Chatter Key -> IO ()) -> GLFW.KeyCallback
keyCallback ref h _ k _ st _ = do
  m <- readIORef h
  m $ case st of
    GLFW.KeyState'Released -> Up (toEnum . fromEnum $ k :: Key)
    _ -> Down (toEnum . fromEnum $ k :: Key)
  case st of
    GLFW.KeyState'Released -> return ()
    _ -> modifyIORef' ref $ \(keys, buf) -> (keys, toEnum (fromEnum k) : buf)

mouseButtonCallback :: IORef (Chatter Int -> IO ()) -> GLFW.MouseButtonCallback
mouseButtonCallback h _ btn st _ = do
  m <- readIORef h
  m $ case st of
    GLFW.MouseButtonState'Released -> Up (fromEnum btn)
    _ -> Down (fromEnum btn)

cursorPosCallback :: IORef (V2 Float -> IO ()) -> GLFW.CursorPosCallback
cursorPosCallback h _ x y = do
  m <- readIORef h
  m $ fmap realToFrac $ V2 x y

scrollCallback :: IORef (V2 Float -> IO ()) -> GLFW.ScrollCallback
scrollCallback h _ x y = do
  m <- readIORef h
  m $ fmap realToFrac $ V2 x y

keyPress :: MonadHolz m => Key -> m Bool
keyPress k = ask >>= \w -> liftIO $ fmap (/=GLFW.KeyState'Released)
  $ GLFW.getKey (theWindow w) (toEnum . fromEnum $ k)

mousePress :: MonadHolz m => Int -> m Bool
mousePress k = ask >>= \w -> liftIO $ fmap (/=GLFW.MouseButtonState'Released)
  $ GLFW.getMouseButton (theWindow w) (toEnum k)

typedString :: MonadHolz m => m String
typedString = ask >>= \w -> liftIO $ fst <$> readIORef (charactersTyped w)

typedKeys :: MonadHolz m => m [Key]
typedKeys = ask >>= \w -> liftIO $ fst <$> readIORef (keysTyped w)

getCursorPos :: MonadHolz m => m (V2 Float)
getCursorPos = ask >>= \w -> liftIO $ fmap realToFrac <$> uncurry V2 <$> GLFW.getCursorPos (theWindow w)

overPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
overPtr f = alloca $ \p -> f p >> peek p
{-# INLINE overPtr #-}

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
  \uniform vec3 specular; \
  \void main(void){ \
  \  fragColor = texture(tex, texUV) * color * diffuse; \
  \}"

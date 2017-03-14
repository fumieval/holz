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
  , closeWindow
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
  , drawVertexBuffer
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
  -- * Internal
  , overPtr
  , getUniform
  , compileShader
  ) where

import Codec.Picture
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Iter
import Data.Bits
import Data.BoundingBox as Box
import Data.IORef
import Foreign
import Foreign.C.String
import Graphics.GL
import Linear
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Holz.Input
import System.Mem.Weak
import System.IO.Unsafe
import Control.Applicative

data Window = Window
  { refRegion :: {-# UNPACK #-} !(IORef (Box V2 Float))
  , theWindow :: {-# UNPACK #-} !GLFW.Window
  , keyboardHandlers :: {-# UNPACK #-} !(IORef (Chatter Key -> IO ()))
  , mouseButtonHandlers :: {-# UNPACK #-} !(IORef (Chatter Int -> IO ()))
  , mouseCursorHandlers :: {-# UNPACK #-} !(IORef (V2 Float -> IO ()))
  , mouseScrollHandlers :: {-# UNPACK #-} !(IORef (V2 Float -> IO ()))
  , keysTyped :: !(IORef ([Key], [Key]))
  , charactersTyped :: !(IORef ([Char], [Char]))
  }

type MonadHolz m = (MonadIO m, MonadReader Window m)

newtype HolzT m a = HolzT { unHolzT :: ReaderT Window (IterT m) a }
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
withFrame m = ask >>= flip withFrameOn m

withFrameOn :: MonadIO m => Window -> m a -> m a
withFrameOn win m = do
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
runHolzT win = go . flip runReaderT win . unHolzT where
  go m = join $ withFrameOn win $ liftIO (GLFW.windowShouldClose (theWindow win)) >>= \case
    False -> either (return . Just) (delay . go) <$> lift (runIterT m)
    True -> return (return Nothing)

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

  initializeGL

  rbox <- newIORef $ bbox & Box.size zero .~ fmap fromIntegral (V2 fw fh)

  hk <- newIORef (const (return ()))
  hb <- newIORef (const (return ()))
  hc <- newIORef (const (return ()))
  hs <- newIORef (const (return ()))
  refKeys <- newIORef ([], [])
  refChars <- newIORef ([], [])

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

  return $ Window rbox win hk hb hc hs refKeys refChars

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

initializeGL :: IO ()
initializeGL = do
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glClearColor 0 0 0 1
  glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
  glDepthFunc GL_LEQUAL
  glDepthMask GL_TRUE
  glDisable GL_CULL_FACE
  glEnable GL_DEPTH_TEST
  glEnable GL_LINE_SMOOTH

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

-- | Set a viewport.
setViewport :: MonadIO m => Box V2 Int -> m ()
setViewport (Box (V2 x0 y0) (V2 x1 y1)) = liftIO $ glViewport
  (fromIntegral x0)
  (fromIntegral y0)
  (fromIntegral $ x1 - x0)
  (fromIntegral $ y1 - y0)

-- | Draw 'VertexBuffer' using the w 'Texture' and a model matrix.
drawVertexBuffer :: MonadIO m => Texture -> VertexBuffer -> m ()
drawVertexBuffer (Texture tex) (VertexBuffer vao vbo m n) = liftIO $ do
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
clearColor :: MonadIO m => V4 Float -> m ()
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
takeScreenshot = ask >>= \win -> liftIO $ do
  V2 w h <- fmap floor <$> view (Box.size zero) <$> readIORef (refRegion win)
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

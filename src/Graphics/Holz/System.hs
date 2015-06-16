{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Graphics.Holz.System (System
  , withFrame
  , withHolz
  , WindowMode(..)
  -- * Graphic
  , Texture
  , registerTexture
  , registerTextures
  , releaseTexture
  , Vertex(..)
  , PrimitiveMode(..)
  , VertexBuffer
  , registerVertex
  , releaseVertex
  , setProjection
  , drawVertex
  -- * Input
  , linkKeyboard
  , linkMouseButton
  , linkMouseCursor
  , linkMouseScroll
  , keyPress
  -- * Misc
  , takeScreenshot
  , setTitle
  , enableCursor
  , disableCursor
  , hideCursor
  , clearColor
  , getBoundingBox
  , setBoundingBox
  , windowShouldClose
  ) where

import Codec.Picture
import Control.Lens
import Control.Monad
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
import Control.Applicative

data System = System
  { refRegion :: IORef (Box V2 Float)
  , theWindow :: GLFW.Window
  , theProgram :: GLuint
  , locationModel :: GLint
  , locationProjection :: GLint
  , locationDiffuse :: GLint
  , locationSpecular :: GLint
  , keyboardHandlers :: IORef (Chatter Key -> IO ())
  , mouseButtonHandlers :: IORef (Chatter Int -> IO ())
  , mouseCursorHandlers :: IORef (V2 Float -> IO ())
  , mouseScrollHandlers :: IORef (V2 Float -> IO ())
  }

linkKeyboard :: Given System => (Chatter Key -> IO ()) -> IO ()
linkKeyboard f = modifyIORef (keyboardHandlers given) (liftA2 (>>) f)

linkMouseButton :: Given System => (Chatter Int -> IO ()) -> IO ()
linkMouseButton f = modifyIORef (mouseButtonHandlers given) (liftA2 (>>) f)

linkMouseCursor :: Given System => (V2 Float -> IO ()) -> IO ()
linkMouseCursor f = modifyIORef (mouseCursorHandlers given) (liftA2 (>>) f)

linkMouseScroll :: Given System => (V2 Float -> IO ()) -> IO ()
linkMouseScroll f = modifyIORef (mouseScrollHandlers given) (liftA2 (>>) f)

data PrimitiveMode = Triangles | TriangleFan | TriangleStrip | LineStrip | LineLoop

data WindowMode = FullScreen | Resizable | Windowed deriving (Show, Eq, Ord)

windowShouldClose :: Given System => IO Bool
windowShouldClose = GLFW.windowShouldClose (theWindow given)

withFrame :: Given System => IO a -> IO a
withFrame m = do
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  a <- m
  GLFW.swapBuffers $ theWindow given
  GLFW.pollEvents
  return a

withHolz :: WindowMode -> Box V2 Float -> (Given System => IO a) -> IO a
withHolz windowmode bbox@(Box (V2 x0 y0) (V2 x1 y1)) m = do
  Encoding.setForeignEncoding Encoding.utf8
  let title = ""
      ww = floor $ x1 - x0
      wh = floor $ y1 - y0

  GLFW.init >>= \r -> case r of
    False -> fail "Failed to initialize"
    True -> return ()

  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'Resizable (windowmode == Resizable)

  mon <- if windowmode == FullScreen then GLFW.getPrimaryMonitor else return Nothing
  win <- GLFW.createWindow ww wh title mon Nothing
    >>= maybe (fail "Failed to create a window") return

  GLFW.makeContextCurrent (Just win)

  GLFW.swapInterval 1

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

  with (V4 1 1 1 1 :: V4 Float) $ \ptr -> glUniform4fv locD 1 (castPtr ptr)
  with (V4 1 1 1 1 :: V4 Float) $ \ptr -> glUniform4fv locS 1 (castPtr ptr)

  a <- give (System rbox win prog locM locP locD locS hk hb hc hs) $ do
    GLFW.setFramebufferSizeCallback win $ Just
      $ \_ w h -> modifyIORef rbox $ Box.size zero .~ fmap fromIntegral (V2 w h)
    GLFW.setKeyCallback win $ Just keyCallback
    GLFW.setMouseButtonCallback win $ Just mouseButtonCallback
    GLFW.setCursorPosCallback win $ Just cursorPosCallback
    GLFW.setScrollCallback win $ Just scrollCallback
    m

  GLFW.destroyWindow win
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

vertexAttributes :: IO ()
vertexAttributes = do
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride pos'
  glEnableVertexAttribArray 1

  glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE stride pos''
  glEnableVertexAttribArray 2
  where
    stride = fromIntegral $ sizeOf (undefined :: Vertex)
    pos' = nullPtr `plusPtr` sizeOf (0 :: V3 CFloat)
    pos'' = pos' `plusPtr` sizeOf (0 :: V2 CFloat)
{-# INLINE vertexAttributes #-}

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
  glEnable GL_CULL_FACE
  glEnable GL_DEPTH_TEST
  glEnable GL_FRAMEBUFFER_SRGB
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

data Vertex = Vertex {-# UNPACK #-} !(V3 Float) {-# UNPACK #-} !(V2 Float) {-# UNPACK #-} !(V3 Float)

align1 :: Int
align1 = sizeOf (undefined :: V3 Float)

align2 :: Int
align2 = align1 + sizeOf (undefined :: V2 Float)

instance Storable Vertex where
  sizeOf _ = sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float) + sizeOf (undefined :: V3 Float)
  {-# INLINE sizeOf #-}
  alignment _ = 0
  {-# INLINE alignment #-}
  peek ptr = Vertex
    <$> peek ptr'
    <*> peek (ptr' `plusPtr` align1)
    <*> peek (ptr' `plusPtr` align2)
    where ptr' = castPtr ptr
  {-# INLINE peek #-}
  poke ptr (Vertex v t n) = do
    poke ptr' v
    poke (ptr' `plusPtr` align1) t
    poke (ptr' `plusPtr` align2) n
    where ptr' = castPtr ptr
  {-# INLINE poke #-}

-- | If a 'VertexBuffer' is considered to be unreachable, then it will be released.
data VertexBuffer = VertexBuffer !GLuint !GLuint !GLenum !GLsizei

-- | If a 'Texture' is considered to be unreachable, then it will be released.
data Texture = Texture !GLuint

-- | Send an image into the graphics driver.
registerTexture :: Image PixelRGBA8 -> IO Texture
registerTexture img@(Image w h _) = registerTextures (V2 w h) [(V2 0 0, img)]

-- | Send a set of images into the graphics driver.
registerTextures :: V2 Int -> [(V2 Int, Image PixelRGBA8)] -> IO Texture
registerTextures (V2 sw sh) imgs = do
  tex <- overPtr (glGenTextures 1)
  glBindTexture GL_TEXTURE_2D tex
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glPixelStorei GL_UNPACK_ALIGNMENT 1
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_LSB_FIRST 0
  glPixelStorei GL_UNPACK_ROW_LENGTH 0
  glPixelStorei GL_UNPACK_SKIP_IMAGES 0
  glPixelStorei GL_UNPACK_SKIP_PIXELS 0
  glPixelStorei GL_UNPACK_SKIP_ROWS 0
  glPixelStorei GL_UNPACK_SWAP_BYTES 0
  let level = floor $ logBase (2 :: Float) $ fromIntegral (max sw sh)
  glTexStorage2D GL_TEXTURE_2D level GL_SRGB8 (fromIntegral sw) (fromIntegral sh)

  when gl_EXT_texture_filter_anisotropic
    $ glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT 8

  forM_ imgs $ \(V2 x y, Image w h vec) -> V.unsafeWith vec
    $ glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE
    . castPtr

  glGenerateMipmap GL_TEXTURE_2D

  let t = Texture tex
  addFinalizer t (releaseTexture t)
  return t

releaseTexture :: Texture -> IO ()
releaseTexture (Texture i) = with i $ glDeleteTextures 1

-- | Send vertices to the graphics driver.
registerVertex :: PrimitiveMode -> V.Vector Vertex -> IO VertexBuffer
registerVertex mode vs = do
  vao <- overPtr $ glGenVertexArrays 1
  glBindVertexArray vao
  vbo <- overPtr $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  vertexAttributes
  let siz = fromIntegral $ V.length vs * sizeOf (undefined :: Vertex)
  V.unsafeWith vs $ \v -> glBufferData GL_ARRAY_BUFFER siz (castPtr v) GL_STATIC_DRAW
  let vb = VertexBuffer vao vbo (convPrimitiveMode mode) (fromIntegral $ V.length vs)
  addFinalizer vb (releaseVertex vb)
  return vb

releaseVertex :: VertexBuffer -> IO ()
releaseVertex (VertexBuffer vao vbo _ _) = do
  with vao $ glDeleteVertexArrays 1
  with vbo $ glDeleteBuffers 1

setProjection :: Given System => M44 Float -> IO ()
setProjection proj = with proj
  $ \ptr -> glUniformMatrix4fv (locationProjection given) 1 1 $ castPtr ptr

drawVertex :: Given System => M44 Float -> Texture -> VertexBuffer -> IO ()
drawVertex mat (Texture tex) (VertexBuffer vao vbo m n) = do
  with mat $ \p -> glUniformMatrix4fv (locationModel given) 1 1 (castPtr p)
  glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  glDrawArrays m 0 n

hideCursor :: Given System => IO ()
hideCursor = GLFW.setCursorInputMode (theWindow given) GLFW.CursorInputMode'Hidden

disableCursor :: Given System => IO ()
disableCursor = GLFW.setCursorInputMode (theWindow given) GLFW.CursorInputMode'Disabled

enableCursor :: Given System => IO ()
enableCursor = GLFW.setCursorInputMode (theWindow given) GLFW.CursorInputMode'Normal

clearColor :: Given System => V4 Float -> IO ()
clearColor (V4 r g b a) = glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

setBoundingBox :: Given System => Box V2 Float -> IO ()
setBoundingBox box@(Box (V2 x0 y0) (V2 x1 y1)) = do
  GLFW.setWindowSize (theWindow given) (floor (x1 - x0)) (floor (y1 - y0))
  writeIORef (refRegion given) box

getBoundingBox :: Given System => IO (Box V2 Float)
getBoundingBox = readIORef (refRegion given)

takeScreenshot :: Given System => IO (Image PixelRGBA8)
takeScreenshot = do
  V2 w h <- fmap floor <$> view (Box.size zero) <$> readIORef (refRegion given)
  mv <- MV.unsafeNew $ w * h * 4 :: IO (MV.IOVector Word8)
  mv' <- MV.unsafeNew $ w * h * 4
  glReadBuffer GL_FRONT
  MV.unsafeWith mv $ \ptr -> do
    glReadPixels 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE (castPtr ptr)
    MV.unsafeWith mv' $ \dst -> forM_ [0..h-1] $ \y -> copyBytes (plusPtr dst $ y * w * 4) (plusPtr ptr $ (h - y - 1) * w * 4) (4 * w)
  Image w h <$> V.unsafeFreeze mv'

setTitle :: Given System => String -> IO ()
setTitle str = GLFW.setWindowTitle (theWindow given) str

keyCallback :: Given System => GLFW.KeyCallback
keyCallback _ k _ st _ = do
  m <- readIORef (keyboardHandlers given)
  m $ case st of
    GLFW.KeyState'Released -> Up (toEnum . fromEnum $ k :: Key)
    _ -> Down (toEnum . fromEnum $ k :: Key)

mouseButtonCallback :: Given System => GLFW.MouseButtonCallback
mouseButtonCallback _ btn st _ = do
  m <- readIORef (mouseButtonHandlers given)
  m $ case st of
    GLFW.MouseButtonState'Released -> Up (fromEnum btn)
    _ -> Down (fromEnum btn)

cursorPosCallback :: Given System => GLFW.CursorPosCallback
cursorPosCallback _ x y = do
  m <- readIORef (mouseCursorHandlers given)
  m $ fmap realToFrac $ V2 x y

scrollCallback :: Given System => GLFW.ScrollCallback
scrollCallback _ x y = do
  m <- readIORef (mouseScrollHandlers given)
  m $ fmap realToFrac $ V2 x y

keyPress :: Given System => Key -> IO Bool
keyPress k = fmap (/=GLFW.KeyState'Released)
  $ GLFW.getKey (theWindow given) (toEnum . fromEnum $ k)

overPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
overPtr f = alloca $ \p -> f p >> peek p
{-# INLINE overPtr #-}

vertexShaderSource :: String
vertexShaderSource = "#version 400\n\
  \uniform mat4 projection; \
  \uniform mat4 model; \
  \in vec3 in_Position; \
  \in vec2 in_UV; \
  \in vec3 in_Normal; \
  \out vec2 texUV; \
  \out vec3 normal; \
  \out vec4 viewPos; \
  \void main(void) { \
  \  viewPos = model * vec4(in_Position, 1.0); \
  \  gl_Position = projection * viewPos; \
  \  texUV = in_UV; \
  \  normal = in_Normal;\
  \}"

fragmentShaderSource :: String
fragmentShaderSource = "#version 400\n\
  \out vec4 fragColor; \
  \in vec2 texUV; \
  \in vec3 normal; \
  \in vec4 viewPos; \
  \uniform sampler2D tex; \
  \uniform vec4 diffuse; \
  \uniform vec3 specular; \
  \void main(void){ \
  \  fragColor = texture(tex, texUV) * diffuse; \
  \}"

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
  , HasWindow(..)
  , MonadHolz
  , runHolz
  , retract
  -- * Drawing region
  , setViewport
  , getBoundingBox
  , setBoundingBox
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
import Graphics.GL
import Linear
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Holz.Input
import Control.Applicative

data Window = Window
  { refRegion :: {-# UNPACK #-} !(IORef (Box V2 Float))
  , refWindowSize :: {-# UNPACK #-} !(IORef (V2 Int))
  , theWindow :: {-# UNPACK #-} !GLFW.Window
  , keyboardHandlers :: {-# UNPACK #-} !(IORef (Chatter Key -> IO ()))
  , mouseButtonHandlers :: {-# UNPACK #-} !(IORef (Chatter Int -> IO ()))
  , mouseCursorHandlers :: {-# UNPACK #-} !(IORef (V2 Float -> IO ()))
  , mouseScrollHandlers :: {-# UNPACK #-} !(IORef (V2 Float -> IO ()))
  , keysTyped :: !(IORef ([Key], [Key]))
  , charactersTyped :: !(IORef ([Char], [Char]))
  }

type MonadHolz r m = (MonadIO m, MonadReader r m)

class HasWindow r where
  getWindow :: r -> Window

instance HasWindow Window where
  getWindow = id

askWindow :: (MonadHolz r m, HasWindow r) => m Window
askWindow = asks getWindow

-- | Set a handler for key events.
linkKeyboard :: (MonadHolz r m, HasWindow r) => (Chatter Key -> IO ()) -> m ()
linkKeyboard f = askWindow >>= \s -> liftIO $ modifyIORef (keyboardHandlers s) (liftA2 (>>) f)

-- | Set a handler for mouse button events.
linkMouseButton :: (MonadHolz r m, HasWindow r) => (Chatter Int -> IO ()) -> m ()
linkMouseButton f = askWindow >>= \s -> liftIO $ modifyIORef (mouseButtonHandlers s) (liftA2 (>>) f)

-- | Set a handler for cursor movement.
linkMouseCursor :: (MonadHolz r m, HasWindow r) => (V2 Float -> IO ()) -> m ()
linkMouseCursor f = askWindow >>= \s -> liftIO $ modifyIORef (mouseCursorHandlers s) (liftA2 (>>) f)

-- | Set a handler for scroll events.
linkMouseScroll :: (MonadHolz r m, HasWindow r) => (V2 Float -> IO ()) -> m ()
linkMouseScroll f = askWindow >>= \s -> liftIO $ modifyIORef (mouseScrollHandlers s) (liftA2 (>>) f)

data WindowMode = FullScreen | Resizable | Windowed deriving (Show, Eq, Ord)

-- | Returns 'True' when the window should be closed.
windowShouldClose :: (MonadHolz r m, HasWindow r) => m Bool
windowShouldClose = askWindow >>= \s -> liftIO $ GLFW.windowShouldClose (theWindow s)

-- | Run actions for a window.
withFrame :: MonadIO m => Window -> m a -> m a
withFrame win m = do
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
runHolz :: (MonadHolz r m, HasWindow r) => IterT m a -> IterT m (Maybe a)
runHolz m0 = asks getWindow >>= go m0 where
  go m win = join $ withFrame win $ liftIO (GLFW.windowShouldClose (theWindow win)) >>= \case
    False -> either (return . Just) (delay . flip go win) <$> lift (runIterT m)
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
  rwin <- newIORef (V2 ww wh)

  let win' = Window rbox rwin win hk hb hc hs refKeys refChars

  GLFW.setFramebufferSizeCallback win $ Just
    $ \_ w h -> do
      GLFW.makeContextCurrent $ Just win
      modifyIORef rbox $ Box.size zero .~ fmap fromIntegral (V2 w h)
      glViewport 0 0 (fromIntegral w) (fromIntegral h)
  GLFW.setWindowSizeCallback win $ Just $ \_ w h -> writeIORef rwin $ V2 w h
  GLFW.setKeyCallback win $ Just $ keyCallback refKeys hk
  GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback hb
  GLFW.setCursorPosCallback win $ Just $ cursorPosCallback win' hc
  GLFW.setScrollCallback win $ Just $ scrollCallback hs
  GLFW.setCharCallback win $ Just $ \_ ch -> modifyIORef' refChars $ \(str, buf) -> (str, ch : buf)

  return win'

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

-- | Set a viewport.
setViewport :: (MonadHolz r m, HasWindow r) => Box V2 Float -> m ()
setViewport (Box (V2 x0 y0) (V2 x1 y1)) = askWindow >>= \w -> liftIO $ do
  V2 _ h <- view (Box.size zero) <$> readIORef (refRegion w)
  glViewport
    (floor x0)
    (floor $ h - y1)
    (floor $ x1 - x0)
    (floor $ y1 - y0)

-- | Hide the system cursor.
hideCursor :: (MonadHolz r m, HasWindow r) => m ()
hideCursor = askWindow >>= \w -> liftIO $ GLFW.setCursorInputMode (theWindow w) GLFW.CursorInputMode'Hidden

-- | Hide and pin the cursor against the program.
disableCursor :: (MonadHolz r m, HasWindow r) => m ()
disableCursor = askWindow >>= \w -> liftIO $ GLFW.setCursorInputMode (theWindow w) GLFW.CursorInputMode'Disabled

-- | Re-enable the cursor.
enableCursor :: (MonadHolz r m, HasWindow r) => m ()
enableCursor = askWindow >>= \w -> liftIO $ GLFW.setCursorInputMode (theWindow w) GLFW.CursorInputMode'Normal

-- | Set the background color.
clearColor :: MonadIO m => V4 Float -> m ()
clearColor (V4 r g b a) = liftIO $ glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- | Set the size of the window and the resolution.
setBoundingBox :: (MonadHolz r m, HasWindow r) => Box V2 Float -> m ()
setBoundingBox box@(Box (V2 x0 y0) (V2 x1 y1)) = do
  w <- askWindow
  liftIO $ GLFW.setWindowSize (theWindow w) (floor (x1 - x0)) (floor (y1 - y0))
  liftIO $ writeIORef (refRegion w) box

getBoundingBox :: (MonadHolz r m, HasWindow r) => m (Box V2 Float)
getBoundingBox = askWindow >>= \w -> liftIO $ readIORef (refRegion w)

-- | Take a screenshot of the window.
takeScreenshot :: (MonadHolz r m, HasWindow r) => m (Image PixelRGBA8)
takeScreenshot = askWindow >>= \win -> liftIO $ do
  V2 w h <- fmap floor <$> view (Box.size zero) <$> readIORef (refRegion win)
  mv <- MV.unsafeNew $ w * h * 4 :: IO (MV.IOVector Word8)
  mv' <- MV.unsafeNew $ w * h * 4
  glReadBuffer GL_FRONT
  MV.unsafeWith mv $ \ptr -> do
    glReadPixels 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE (castPtr ptr)
    MV.unsafeWith mv' $ \dst -> forM_ [0..h-1] $ \y -> copyBytes (plusPtr dst $ y * w * 4) (plusPtr ptr $ (h - y - 1) * w * 4) (4 * w)
  Image w h <$> V.unsafeFreeze mv'

-- | Set the window title.
setTitle :: (MonadHolz r m, HasWindow r) => String -> m ()
setTitle str = askWindow >>= \w -> liftIO $ GLFW.setWindowTitle (theWindow w) str

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

cursorPosCallback :: Window -> IORef (V2 Float -> IO ()) -> GLFW.CursorPosCallback
cursorPosCallback w h _ x y = do
  m <- readIORef h
  p <- transformPos w (V2 x y)
  m p

scrollCallback :: IORef (V2 Float -> IO ()) -> GLFW.ScrollCallback
scrollCallback h _ x y = do
  m <- readIORef h
  m $ fmap realToFrac $ V2 x y

keyPress :: (MonadHolz r m, HasWindow r) => Key -> m Bool
keyPress k = askWindow >>= \w -> liftIO $ fmap (/=GLFW.KeyState'Released)
  $ GLFW.getKey (theWindow w) (toEnum . fromEnum $ k)

mousePress :: (MonadHolz r m, HasWindow r) => Int -> m Bool
mousePress k = askWindow >>= \w -> liftIO $ fmap (/=GLFW.MouseButtonState'Released)
  $ GLFW.getMouseButton (theWindow w) (toEnum k)

typedString :: (MonadHolz r m, HasWindow r) => m String
typedString = askWindow >>= \w -> liftIO $ fst <$> readIORef (charactersTyped w)

typedKeys :: (MonadHolz r m, HasWindow r) => m [Key]
typedKeys = askWindow >>= \w -> liftIO $ fst <$> readIORef (keysTyped w)

transformPos :: Window -> V2 Double -> IO (V2 Float)
transformPos w v = do
  siz <- readIORef (refWindowSize w)
  siz' <- view (Box.size zero) <$> readIORef (refRegion w)
  return $! (\p s t -> realToFrac p / fromIntegral s * t) <$> v <*> siz <*> siz'

getCursorPos :: (MonadHolz r m, HasWindow r) => m (V2 Float)
getCursorPos = askWindow >>= \w -> liftIO $ do
  pos <- uncurry V2 <$> GLFW.getCursorPos (theWindow w)
  transformPos w $! pos

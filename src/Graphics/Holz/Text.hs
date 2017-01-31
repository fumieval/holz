{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables, LambdaCase, GADTs, FlexibleContexts, BangPatterns, TemplateHaskell, DeriveFunctor #-}
---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Text rendering class
-- Qualified import is recommended.
---------------------------------------------------------------------------
module Graphics.Holz.Text (
  typewriter
  , WriterState(..)
  , toDraw
  , offset
  , cache
  , Writing
  , Renderer
  , runRenderer
  , render
  , clear
  , string
  , getOffset
  , simpleL
  , simpleR
  ) where
import Codec.Picture
import Control.Concurrent.MVar
import Control.Lens hiding (simple)
import Data.Foldable
import Data.Reflection (give)
import Data.Tuple
import Graphics.Holz
import Linear
import Graphics.Holz.Vertex
import Data.Map.Strict as Map
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

data WriterState = WriterState
  { font :: Font
  , _toDraw :: [(V2 Float, Texture, VertexBuffer)]
  , _offset :: !(V2 Float)
  , _cache :: !(Map.Map (Float, Char, V4 Float) (Texture, VertexBuffer, V2 Float)) }
makeLenses ''WriterState

-- | A 'Renderer' handles 'Writing' operations.
type Renderer = MVar WriterState

-- | The 'Writing' monad is the interface of text rendering.
--  'string', 'render', 'clear', 'getOffset' etc through ('..-').
--
-- @renderer ..- 'simpleL' 12 (V4 1 1 1 1) "Hello, world" 'identity'@
--
type Writing = StateT WriterState (ShaderT IO)

-- | Render a 'String'.
-- The left edge of the baseline will be at @mat !* V4 0 0 0 1@.
simpleL :: Float -- Size
  -> V4 Float -- ^ Color (RGBA)
  -> String -- String
  -> M44 Float -- Matrix
  -> Writing ()
simpleL s col str m = do
  string s col str
  render m
  clear

-- | Draw a 'String', right-aligned.
-- The right edge of the baseline will be at @mat !* V4 0 0 0 1@.
simpleR :: Float -> V4 Float -> String -> M44 Float -> Writing ()
simpleR s col str m = do
  string s col str
  V2 x y <- getOffset
  render $ m !*! set translation (V3 (-x) (-y) 0) identity
  clear

-- | Render the text to the window, applying a model matrix.
render :: M44 Float -> Writing ()
render m = do
  xs <- use toDraw
  for_ xs $ \(v, tex, buf) -> lift $ drawVertex
    (m !*! (identity & translation . _xy .~ v)) tex buf

-- | Clear the text.
clear :: Writing ()
clear = do
  toDraw .= []
  offset .= V2 0 0

-- | Type one character.
char :: Float -> V4 Float -> Char -> Writing ()
char s col ch = do
  ws <- get
  (tex, buf, adv) <- preuse (cache . ix (s, ch, col)) >>= \case
    Just a -> return a
    Nothing -> do
      (img@(Image w h _), ofs, adv) <- lift $ renderChar (font ws) s ch
      buf <- uncurry registerVertex
          $ rectangle col ofs (ofs + V2 (fromIntegral w) (fromIntegral h))
      tex <- registerTexture img
      cache . at (s, ch, col) ?= (tex, buf, adv)
      return (tex, buf, adv)
  pos <- use offset
  toDraw %= ((pos, tex, buf):)
  offset .= pos + adv

-- | Write a string.
string :: Float -> V4 Float -> String -> Writing ()
string s col = mapM_ (char s col)

-- | Get the current position of writing.
getOffset :: Writing (V2 Float)
getOffset = use offset

-- | Create a renderer of the specified font.
typewriter :: MonadIO m => FilePath -> m Renderer
typewriter path = liftIO $ do
  font <- readFont path
  newMVar $ WriterState font [] zero Map.empty

runRenderer :: MonadIO m => Renderer -> Writing a -> ShaderT m a
runRenderer v m = ShaderT ask >>= \w -> liftIO $ modifyMVar v
  $ \s -> fmap swap $ runShaderT w $ runStateT m s

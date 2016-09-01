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
---------------------------------------------------------------------------
module Graphics.Holz.Text (
  typewriter
  , Writing
  , WritingBase(..)
  , Renderer
  , render
  , clear
  , string
  , getOffset
  , simpleL
  , simpleR
  , (..-)
  ) where
import Codec.Picture
import Control.Concurrent.MVar
import Control.Lens hiding (simple)
import Data.Foldable
import Data.Reflection (give)
import Graphics.Holz
import Linear
import Graphics.Holz.Vertex
import Data.Map.Strict as Map
import Control.Monad.Trans
import Control.Monad.Skeleton
import Control.Object

data WritingBase x where
  TypeChar :: !Char -> !Float -> !(V4 Float) -> WritingBase ()
  Render :: !Window -> !(M44 Float) -> WritingBase ()
  Clear :: WritingBase ()
  GetOffset :: WritingBase (V2 Float)

-- | A 'Renderer' handles 'Writing' operations.
type Renderer = MVar (Object WritingBase IO)

-- | The 'Writing' monad is the interface of text rendering.
--  'string', 'render', 'clear', 'getOffset' etc through ('..-').
--
-- @renderer ..- 'simpleL' 12 (V4 1 1 1 1) "Hello, world" 'identity'@
--
type Writing = Skeleton WritingBase

-- | Render a 'String'.
-- The left edge of the baseline will be at @mat !* V4 0 0 0 1@.
simpleL :: Given Window
  => Float -- Size
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
simpleR :: Given Window => Float -> V4 Float -> String -> M44 Float -> Writing ()
simpleR s col str m = do
  string s col str
  V2 x y <- getOffset
  render $ m !*! set translation (V3 (-x) (-y) 0) identity
  clear

-- | Render the text to the window, applying a model matrix.
render :: Given Window => M44 Float -> Writing ()
render m = bone $ Render given m

-- | Clear the text.
clear :: Writing ()
clear = bone Clear

-- | Type one character.
char :: Float -> V4 Float -> Char -> Writing ()
char s col ch = bone $ TypeChar ch s col

-- | Write a string.
string :: Float -> V4 Float -> String -> Writing ()
string s col = mapM_ (char s col)

-- | Get the current position of writing.
getOffset :: Writing (V2 Float)
getOffset = bone GetOffset

data WriterState = WriterState
  { _toDraw :: [(M44 Float -> M44 Float, Texture, VertexBuffer)]
  , _offset :: !(V2 Float)
  , _cache :: !(Map.Map (Float, Char, V4 Float) (Texture, VertexBuffer, V2 Float)) }
makeLenses ''WriterState

-- | Create a renderer of the specified font.
typewriter :: MonadIO m => FilePath -> m Renderer
typewriter path = liftIO $ do
  font <- readFont path
  newMVar $ WriterState [] zero Map.empty @~ \case
    TypeChar ch s col -> do
      (tex, buf, adv) <- preuse (cache . ix (s, ch, col)) >>= \case
        Just a -> return a
        Nothing -> do
          (img@(Image w h _), ofs, adv) <- lift $ renderChar font s ch
          buf <- uncurry registerVertex
              $ rectangle col ofs (ofs + V2 (fromIntegral w) (fromIntegral h))
          tex <- registerTexture img
          cache . at (s, ch, col) ?= (tex, buf, adv)
          return (tex, buf, adv)
      pos <- use offset
      toDraw %= ((translation . _xy +~ pos, tex, buf):)
      offset .= pos + adv
    Render w m -> give w $ do
      xs <- use toDraw
      for_ xs $ \(t, tex, buf) -> drawVertex (t m) tex buf
    Clear -> do
      toDraw .= []
      offset .= V2 0 0
    GetOffset -> use offset

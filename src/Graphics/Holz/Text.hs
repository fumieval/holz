{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables, LambdaCase, GADTs, FlexibleContexts, BangPatterns, TemplateHaskell, DeriveFunctor #-}
module Graphics.Parkett.Text (
  typewriter
  , Writing
  , Renderer
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
import Graphics.Holz
import Linear
import Graphics.Parkett.Vertex
import Data.Map.Strict as Map
import Control.Monad.Trans
import Control.Monad.Skeleton
import Control.Object

data WritingBase x where
  TypeChar :: !Char -> !Float -> !(V4 Float) -> WritingBase ()
  Render :: !Window -> !(M44 Float) -> WritingBase ()
  Clear :: WritingBase ()
  GetOffset :: WritingBase (V2 Float)

type Renderer = MVar (Object WritingBase IO)
type Writing = Skeleton WritingBase

simpleL :: Given Window => Float -> V4 Float -> String -> M44 Float -> Writing ()
simpleL s col str m = do
  string s col str
  render m
  clear

simpleR :: Given Window => Float -> V4 Float -> String -> M44 Float -> Writing ()
simpleR s col str m = do
  string s col str
  V2 x y <- getOffset
  render $ m !*! set translation (V3 (-x) (-y) 0) identity
  clear

render :: Given Window => M44 Float -> Writing ()
render m = bone $ Render given m

clear :: Writing ()
clear = bone Clear

char :: Float -> V4 Float -> Char -> Writing ()
char s col ch = bone $ TypeChar ch s col

string :: Float -> V4 Float -> String -> Writing ()
string s col = mapM_ (char s col)

getOffset :: Writing (V2 Float)
getOffset = bone GetOffset

data WriterState = WriterState
  { _toDraw :: [(M44 Float -> M44 Float, Texture, VertexBuffer)]
  , _offset :: !(V2 Float)
  , _cache :: !(Map.Map (Float, Char) (Texture, VertexBuffer, V2 Float)) }
makeLenses ''WriterState

typewriter :: MonadIO m => FilePath -> m Renderer
typewriter path = liftIO $ do
  font <- readFont path
  newMVar $ WriterState [] zero Map.empty @~ \case
    TypeChar ch s col -> do
      (tex, buf, adv) <- preuse (cache . ix (s, ch)) >>= \case
        Just a -> return a
        Nothing -> do
          (img@(Image w h _), ofs, adv) <- lift $ renderChar font s ch
          buf <- uncurry registerVertex
              $ rectangle col ofs (ofs + V2 (fromIntegral w) (fromIntegral h))
          tex <- registerTexture img
          cache . at (s, ch) ?= (tex, buf, adv)
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

import Graphics.Holz
import Graphics.Holz.Vertex
import Control.Monad
import Linear

main = withHolz $ do
  win <- openWindow Windowed (Box (V2 0 0) (V2 640 480))
  sh <- makeShader
  retract $ runHolzT win $ forever $ withFrame win $ do
    pos <- getCursorPos
    runShaderT sh $ do
      setOrthographic
      draw identity $ rectangle (pure 1) (pos - V2 10 10) (pos + V2 10 10)

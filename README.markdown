Holz: A graphics library
=================================

This package provides API for developing OpenGL / GLFW-based application.

[API Documentaion](http://hask.moe/doc/holz-0/Graphics-Holz-System.html)

Example
------------------------------

```haskell
import Control.Monad
import Data.Function
import Graphics.Holz
import Linear

main = withHolz $ do
  w <- openWindow Windowed (Box (V2 0 0) (V2 640 480))
  fix $ \self -> do
    b <- withFrame w windowShouldClose
    unless b self
```

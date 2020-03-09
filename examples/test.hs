{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Holz
import Control.Monad
import Linear
import qualified Graphics.Holz.Shader.Simple as S
import Data.Function (fix)
import qualified Text.RawString.QQ as QQ

data WindowAndShader = WindowAndShader
    { wsWindow :: !Window
    , wsShader :: !(Shader Info S.Vertex)
    }

instance HasWindow WindowAndShader where getWindow = wsWindow
instance HasShader WindowAndShader where
  type ShaderUniform WindowAndShader = Info
  type ShaderVertex WindowAndShader = S.Vertex
  getShader = wsShader

data Info h = Info
  { mouse :: h (V2 Float)
  , time :: h Float
  , resolution :: h (V2 Float)
  } deriving Generic

main :: IO ()
main = withHolz $ do
  win <- openWindow Resizable (Box (V2 0 0) (V2 640 480))
  sh <- makeShader vertexShaderSource fragmentShaderSource
  void $ flip runReaderT (WindowAndShader win sh) $ do
    vb <- uncurry registerVertex $ S.rectangle (pure 1) (V2 (-1) (-1)) (V2 1 1)
    retract $ runHolz $ flip fix (0 :: Int) $ \self t -> do
      pos <- getCursorPos
      setUniform mouse pos
      setUniform time (fromIntegral t / 100)
      setUniform resolution (V2 1 1)
      drawVertexBuffer blankTexture vb

      delay $ self $ t + 1

vertexShaderSource :: VertexShaderSource Info S.Vertex S.Fragment
vertexShaderSource = [QQ.r|
void main(void)
{
  fPos = vec4(vPosition, 1.0);
  gl_Position = fPos;
  fTexUV = vUV;
  fColor = vRGBA;
}
|]

fragmentShaderSource :: FragmentShaderSource Info S.Fragment
fragmentShaderSource = [QQ.r|
//https://www.shadertoy.com/view/XsVSzW
out vec4 fragColor;
void main( void )
{
	vec2 uv = ( fPos.xy / resolution.xy )*4.0;

	vec2 uv0=uv;
	float i0=1.2;
	float i1=0.95;
	float i2=1.5;
	vec2 i4=vec2(0.0,0.0);
	for(int s=0;s<5;s++)
	{
		vec2 r;
		r=vec2(cos(uv.y*i0-i4.y+time/i1),sin(uv.x*i0+i4.x+time/i1))/i2;
		r+=vec2(-r.y,r.x)*0.2;
		uv.xy+=r;

		i0*=1.93;
		i1*=1.25;
		i2*=1.7;
		i4+=r.xy*1.0+0.5*time*i1;
	}
	float r=sin(uv.x-time)*0.5+0.5;
	float b=sin(uv.y+time)*0.5+0.5;
	float g=sin((sqrt(uv.x*uv.x+uv.y*uv.y)+time))*0.5+0.5;
	vec3 c=vec3(r,g,b);
	fragColor = vec4(c,1.0);
}
|]


import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

data Complex = C Float Float deriving (Show,Eq)

instance Num Complex where --Num Typeclass
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z w) = C (z*x - y*w) (y*z + x*w)
    (C x y) + (C z w) = C (x+z) (y+w)
    abs (C x y)     = C (sqrt (x*x + y*y)) 0.0

div' :: Complex -> Complex -> Complex  --Div function
div' (C x y) (C z w) = complex ((x * z + y * w) / (z*z + w*w))
                                           ((y * z - x * w) / (z*z + w*w))

complex :: Float -> Float -> Complex
complex = C --Conversion to Complex

real :: Complex -> Float
real (C x y)    = x --Conversion to Real

magnitude :: Complex -> Float
magnitude = real.abs --Magnitude


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] --Double Buffered Mode
  _window <- createWindow "Image2"  --Creating window with name

  windowSize $= Size 1024 768  --Setting Size of Window
  displayCallback $= display --Display function used
  mainLoop


width = 100 :: GLfloat
height = 100 :: GLfloat

display :: DisplayCallback
display = do  --Series of Monadic Statements
  clear [ColorBuffer] --Clearing arbitary parts of other applications in display window
  loadIdentity -- Reset any transformation
  preservingMatrix drawImage
  swapBuffers  -- Commits statements ; used because display mode = double buffered


drawImage =
  -- We will print Points (not triangles for example)
  renderPrimitive Points $ do
    mapM_ drawColoredPoint allPoints
  where
      drawColoredPoint (x,y,c) = do
          color c -- Set the current color to c
          vertex $ Vertex3 x y 0 -- then draw the point at position (x,y,0)


allPoints :: [(GLfloat,GLfloat,Color3 GLfloat)] --Generating List of all Points to be plotted
allPoints = [ (x/width,y/height,colorFromValue $ image x y) |
                  x <- [-width..width],
                  y <- [-height..height]]


colorFromValue n = --Colour of each point
  let
      t :: Int -> GLfloat
      t i = 0.4 + 0.4*cos( fromIntegral i / 10 )
  in
    Color3 (t (n-2)) (t (n+2)) (t (n+6))


image x y =               --Generating Point Co-ords
  let r = 2.0 * x / width
      i = 2.0 * y / height
  in
      f (complex r i) 0 5


f :: Complex -> Complex -> Int -> Int
f c z 0 = 0
f c z n = if (magnitude z > 2 )
          then n
          else f c ( z - div' ((z - 1)^3) (3*(z^2)) + c ) (n-1)


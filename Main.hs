{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data Complex = C Float Float deriving (Show,Eq)

instance Num Complex where --Num Typeclass
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z w) = C (z*x - y*w) (y*z + x*w)
    (C x y) + (C z w) = C (x+z) (y+w)
    abs (C x y)     = C (sqrt (x*x + y*y)) 0.0
    (C x y) - (C z w) = C (x - z) (y - w)

div' :: Complex -> Complex -> Complex  --Div function
div' (C x y) (C z w) = complex ((x * z + y * w) / (z*z + w*w))
                                           ((y * z - x * w) / (z*z + w*w))

exp' :: Complex -> Int -> Complex --Exponent Function
exp' z 0 = 1
exp' z n = z * exp' z (n-1)

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
  _window <- createWindow "Julia Set"  --Creating window with name

  windowSize $= Size 1024 768  --Setting Size of Window
  displayCallback $= display --Display function used
  mainLoop


width :: GLfloat
width = 500 :: GLfloat
height :: GLfloat
height = 500 :: GLfloat

display :: DisplayCallback
display = do  --Series of Monadic Statements
  clearColor $= Color4 1 0.8 0.6 1
  clear [ColorBuffer] --Clearing arbitary parts of other applications in display window
  loadIdentity -- Reset any transformation
  preservingMatrix drawImage
  swapBuffers  -- Commits statements ; used because display mode = double buffered


drawImage :: IO ()
drawImage =
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


redRGB :: Int   --Can be tweaked for diff colours
redRGB = 194

greenRGB :: Int
greenRGB = 30

blueRGB :: Int
blueRGB = 86


colorFromValue :: Int-> Color3 GLfloat 
colorFromValue n = 
    let
      t :: Int -> Int -> GLfloat
      t n i = fromIntegral((n * n * i - i + 50) `mod` 255) / 255 --random colour generation dependant on red/green/blueRGB vals
    in
      Color3 (t n redRGB) (t n greenRGB) (t n blueRGB)


maxIters :: Int
maxIters = 30

image :: Float -> Float -> Int
image x y =               --Generating Point Co-ords
  let r = 1.0 * x / width
      i = 1.0 * y / height
      r' = 1.0 * x / width
      i' = 0.0 * y / height
  in
      f  (complex r' i') (complex r i) 0

f :: Complex -> Complex -> Int -> Int 
f z c iters 
          | iters>maxIters=0
          | magnitude z > 2 = iters
          | otherwise = f ((z^2) + c) c (iters + 1)

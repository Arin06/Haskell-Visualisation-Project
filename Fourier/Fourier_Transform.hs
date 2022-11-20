import Data.Complex
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

type Point = Complex Float
type Edge  = (Point, Point)


traced_points :: [Point]
-- traced_points = [(n:+n) | n <- [0..10]]
traced_points = [(1:+641),(1:+574),(41:+550),(91:+522),(136:+464),(141:+461),(176:+435),(216:+462),(242:+495),(243:+494),(271:+512),(311:+528),(344:+527),(368:+521),(395:+486),(407:+439),(421:+389),(426:+337),(419:+288),(414:+268),(397:+258),(378:+256),(387:+265),(374:+272),(368:+269),(366:+264),(382:+256),(362:+261),(354:+266),(304:+276),(286:+271),(268:+275),(244:+281),(238:+285),(259:+287),(254:+283),(265:+286),(276:+284),(279:+276),(305:+274),(346:+272),(355:+289),(362:+309),(370:+321),(381:+338),(373:+350),(356:+362),(334:+356),(310:+355),(337:+361),(384:+365),(373:+360),(346:+363),(327:+361),(298:+369),(270:+377),(257:+386),(257:+399),(276:+395),(297:+391),(322:+390),(353:+389),(370:+384),(379:+380),(366:+392),(354:+405),(329:+409),(306:+410),(290:+407),(281:+398),(276:+407),(303:+416),(327:+417),(347:+414),(359:+411),(370:+396),(372:+388),(412:+375),(402:+388),(393:+413),(386:+429),(382:+449),(380:+471),(372:+483),(346:+491),(320:+492),(302:+481),(291:+469),(274:+448),(257:+431),(234:+412),(218:+396),(200:+369),(193:+356),(177:+329),(170:+305),(167:+276),(178:+256),(173:+235),(164:+208),(162:+178),(183:+158),(199:+143),(225:+137),(248:+133),(268:+108),(276:+95),(296:+98),(319:+105),(338:+112),(356:+128),(365:+138),(378:+159),(385:+178),(396:+203),(405:+218),(404:+193),(404:+159),(393:+135),(378:+113),(352:+78),(345:+61),(310:+46),(255:+40),(218:+43),(187:+50),(160:+65),(139:+81),(122:+103),(118:+109),(103:+135),(97:+147),(72:+161),(51:+180),(50:+211),(53:+235),(49:+255),(47:+293),(51:+314),(60:+341),(67:+350),(72:+375),(80:+393),(83:+402),(96:+421),(108:+434),(122:+441),(133:+439),(134:+426),(127:+413),(143:+401),(135:+403),(117:+399),(101:+381),(88:+359),(77:+335),(72:+311),(78:+289),(89:+283),(104:+284),(117:+290),(125:+305),(112:+319),(106:+331),(102:+342),(116:+357),(133:+363),(150:+386),(156:+407),(175:+430),(203:+457),(223:+472),(243:+493),(232:+483),(259:+502),(278:+513),(302:+524),(327:+530),(357:+526),(376:+517),(395:+497),(400:+483),(407:+471),(422:+473),(458:+480),(478:+483),(482:+509),(483:+540),(484:+572),(485:+602),(480:+634),(480:+642),(451:+640),(419:+641),(411:+609),(405:+591),(405:+574),(399:+548),(393:+528),(390:+514),(370:+521),(344:+528),(324:+542),(318:+550),(305:+560),(299:+564),(316:+570),(329:+580),(338:+586),(359:+607),(367:+618),(380:+631),(390:+639),(211:+640),(222:+626),(235:+606),(243:+596),(254:+583),(263:+576),(276:+571),(271:+561),(246:+553),(237:+548),(226:+541),(207:+533),(198:+526),(178:+513),(174:+510),(159:+498),(156:+489),(143:+470),(143:+464),(137:+471),(136:+486),(130:+513),(136:+528),(142:+556),(150:+586),(160:+617),(168:+635),(159:+642),(112:+641),(59:+642),(3:+641)]

edges :: [Edge]
edges = [e | e <- zip traced_points 
        (tail traced_points ++ [head traced_points])]

midpoint :: Edge -> Point
midpoint (p1, p2) = (p1 + p2) / 2

num_of_points :: Int
num_of_points = length traced_points

--Generating Fourier Coefficients
nthCoefficient :: Int -> Point
nthCoefficient index = sum [midpoint edge * exp (power i) / num_points
                          | (edge, i) <- zip edges [0 ..]]
                          where power i    = -2 * pi * fromIntegral index * (0:+i) / num_points
                                num_points = fromIntegral num_of_points

nthCoefficientsList :: Int -> [(Int, Point)]
nthCoefficientsList n = [(i, nthCoefficient i) | i <- [-n..n]]

fourierFunction :: Int -> Float -> Point
fourierFunction n t = sum [c * exp (power i) | (i, c) <- nthCoefficientsList n]
                          where power i = -2 * pi * (0:+t) * fromIntegral i

numOfSteps :: Int --Number of Iterations
numOfSteps = 200

pointsDraw :: Int -> [Point] --Generating Points
pointsDraw n = [fourierFunction n (t i) | i <- [0..numOfSteps]]
               where t i = fromIntegral i / fromIntegral numOfSteps

--Setting Upper and Lower Bound of Points
max' :: [Point] -> Float
max' points = foldr1 max (foldr (++) [] [[a,b]| (a:+b) <- points])

min' :: [Point] -> Float
min' points = foldr1 min (foldr (++) [] [[a,b]| (a:+b) <- points])

--Scaling X and Y Co-ord vals
scaling :: [Point] -> [(GLfloat,GLfloat,GLfloat)]
scaling points = [((2*x-lowerBound-upperBound)/interval, (upperBound+lowerBound-y*2)/interval, 0) | (x:+y) <- points]
                 where lowerBound = min' points
                       upperBound = max' points
                       interval   = upperBound - lowerBound

--Points to be Plotted
twoDimensionPoints :: Int -> [(GLfloat,GLfloat,GLfloat)]
twoDimensionPoints n = scaling [point | point <- pointsDraw n]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Don"
  windowSize $= Size 720 720
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive LineLoop $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (twoDimensionPoints 200)  --DO NOT CHANGE THE VALUE!!
  flush

----------------------------------------------
--
-- Cole Turner
-- Haskell GUI experimentation and Optics program
-- April 9, 2018
--
----------------------------------------------

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, yellow, white)
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
  
data Lens = MakeLens {pos, rad, foc :: Float}
type Ray = Path
       
main :: IO()
main = play (InWindow "Haskell Optics" (500,500) (10,10))
       (makeColor 0.0 0.0 0.0 1)
       10
       (0.0,0.0)
       getPicture
       handleEvent
       stepFunction
       
stepFunction :: Float -> (Float, Float) -> (Float, Float)
stepFunction _ p = p

sgn :: Float -> Float
sgn num = num / (abs num)

handleEvent :: Event -> (Float, Float) -> (Float, Float)
handleEvent (EventKey
             (SpecialKey key)
             Down
             Modifiers{shift = Down}
             pos) (a,b) =
  case key of
    KeyUp -> (a, b+2)
    KeyDown -> (a, b-2)
    KeyLeft -> (a-2, b)
    KeyRight -> (a+2, b)
    _ -> (a, b)
handleEvent e p = p                                                                                
  
getRays :: (Float, Float) -> [Ray]
getRays (a,b) = map (\x -> rayTrace x getLenses)
                $ map (\(x,y) -> (x+a,y+b):(a,b):[])
                $ map (\x -> (unitVectorAtAngle ((x+0.1)*pi/(2*n)))) [(-n)..(n-1)]
  where n = 20

getPicture :: (Float, Float) -> Picture
getPicture p = translate (-500) 0 (Pictures [ graphicAxis
                         , Pictures (map graphicLens getLenses)
                         , Pictures (map graphicRay $ getRays p)
                         ])
        
getLenses :: [Lens]
getLenses = [ MakeLens 90 70 50
            , MakeLens 200 50 (-50)
            , MakeLens 400 100 100
            ]

rayTrace :: Ray -> [Lens] -> Ray
rayTrace (p:ps) [] = (fst (head ps) + s*(fst p - fst (head ps)), snd (head ps) + s*(snd p - snd (head ps))):ps
  where s = 10000
rayTrace (p:ps) (l:ls) = rayTrace i ls
  where o = fromMaybe (fst p + 1, snd p) (getImage p l)
        i = case getIntersect p (head ps) l of
              Nothing -> (p:ps)
              Just (a,b) -> ((closestPointOnLine (a,b) o (a+1,b)):(a,b):ps)

getIntersect :: Point -> Point -> Lens -> Maybe Point
getIntersect p1 p2 l = intersectSegLine (pos l, -(rad l)) (pos l, rad l) p1 p2

getImage :: Point -> Lens -> Maybe Point
getImage p l = intersectLineLine (pos l, snd p) (pos l + foc l, 0) p (pos l, 0)

graphicAxis :: Picture
graphicAxis = Color yellow (Line [(-1000,0),(1000,0)])

graphicRay :: Ray -> Picture
graphicRay = Color yellow . Line . reverse

graphicLens :: Lens -> Picture
graphicLens l = Color white $ Pictures [
  Line [(pos l, rad l), (pos l, -(rad l))]
  , translate (pos l + foc l) 0 $ focalPoint
  , translate (pos l - foc l) 0 $ focalPoint
  , translate (pos l) (rad l * shape) $ upArrow
  , translate (pos l) (-rad l * shape) $ downArrow
  ]
  where upArrow = scale 5 5 $ Line [(-1,-1),(0,0),(1,-1)]
        downArrow = scale 5 5 $ Line [(-1,1),(0,0),(1,1)]
        shape = sgn $ foc l
        focalPoint = ThickCircle 1 2        


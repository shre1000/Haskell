module Fun_7_module_point
(Point, newpt, getx, gety)
where

type Point = (Double,Double)

newpt :: Double -> Double -> Point
newpt x y = (x,y)

getx :: Point -> Double
getx (x,y) = x

gety :: Point -> Double
gety (x,y) = y

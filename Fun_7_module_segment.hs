module Fun_7_module_segment
(Segment, startpt, endpt, newseg, showseg, midpt)
where
import Fun_7_module_point

type Segment = (Point, Point)

newseg :: Point -> Point -> Segment
newseg (x1,y1) (x2,y2) = ((x1,y1),(x2,y2))

startpt :: Segment -> Point
startpt ((x1,y1),(x2,y2)) = (x1,y1)

endpt :: Segment -> Point
endpt ((x1,y1),(x2,y2)) = (x2,y2)

showseg :: Segment -> String
showseg s = "The start point of line segment is " ++ show(startpt s) ++ " and end point of line segment is " ++ show(endpt s)

midpt :: Segment -> Point
midpt s = newpt ((getx(startpt(s))+getx(endpt(s)))/2) ((gety(startpt(s))+gety(endpt(s)))/2)

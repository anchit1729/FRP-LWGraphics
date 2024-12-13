module Primitive (Primitive(Rectangle, Ellipse, RtTriangle, Polygon),
                  Radius, Side, Vertex,
                  square, circle, distBetween,
                  ) where 

data Primitive = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
    deriving Show 

type Radius = Float
type Side = Float
type Vertex = (Float, Float)

square s = Rectangle s s

circle r = Ellipse r r

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)


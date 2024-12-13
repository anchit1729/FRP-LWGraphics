module Region ( Region (Primitive, Translate, Scale, Complement,
                        Union, Intersect, Xor, Empty),
                Coordinate,
                containsS, containsR
              ) where 

import Primitive

infixr 5 `Union`
infixr 6 `Intersect`

data Region = Primitive Primitive 
            | Translate Vector Region 
            | Scale Vector Region 
            | Complement Region 
            | Region `Union` Region 
            | Region `Intersect` Region 
            | Region `Xor` Region 
            | Empty 
    deriving Show 

type Vector = (Float, Float)

type Coordinate = (Float, Float)

isToLeftOf :: Coordinate -> Ray -> Bool 
(px, py) `isToLeftOf` ((ax, ay), (bx, by))
        = let (s, t) = (px - ax, py - ay)
              (u, v) = (px - bx, py - by)
          in s * v >= t * u 

type Ray = (Coordinate, Coordinate)

containsR :: Region -> Coordinate -> Bool 
(Primitive p) `containsR` x = p `containsS` x
(Translate (u, v) r) `containsR` (x, y) = let p = (x - u, y - v) in r `containsR` p
(Scale (u, v) r) `containsR` (x, y) = let p = (x / u, y / v) in r `containsR` p
(Complement r) `containsR` p = not (r `containsR` p)
(r1 `Union` r2) `containsR` p = r1 `containsR` p || r2 `containsR` p 
(r1 `Intersect` r2) `containsR` p = r1 `containsR` p && r2 `containsR` p 
(r1 `Xor` r2) `containsR` p
    = let a = r1 `containsR` p
          b = r2 `containsR` p 
      in (a || b) && not (a && b)
Empty `containsR` p = False 

containsS :: Primitive -> Coordinate -> Bool
(Rectangle s1 s2) `containsS` (x, y)
    = let t1 = s1 / 2; t2 = s2 / 2
      in (-t1 <= x) && (x <= t1) && (-t2 <= y) && (y <= t2)
(Ellipse r1 r2) `containsS` (x, y)
    = (x/r1)^2 + (y/r2)^2 <= 1 
(Polygon pts) `containsS` p
    = let leftOfList = map (isToLeftOf p)
                           (zip pts (tail pts ++ [head pts]))
      in and leftOfList 
(RtTriangle s1 s2) `containsS` p = (Polygon [(0,0), (s1, 0), (0, s2)]) `containsS` p

universalSet = Complement Empty 


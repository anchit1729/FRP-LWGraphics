module Viewport where 

import GLFWWrapper hiding (circle)
import Primitive

-- remember, 224 ppi is the pixel density on the MacBook Air 
inchToPixel :: Float -> Int
inchToPixel val = round(100 * val)

-- same as above
pixToInch :: Int -> Float 
pixToInch pix = (fromInteger (toInteger pix)) / 100

-- define the window dimensions (keep this fixed until the end, add movement only if time left)
winWidth, winHeight :: Int 
winWidth = 1200 
winHeight = 1000 

halfWinWidth, halfWinHeight :: Int 
halfWinWidth = winWidth `div` 2
halfWinHeight = winHeight `div` 2

trans :: Vertex -> Point 
trans (x, y) = (halfWinWidth + inchToPixel x, 
                halfWinHeight - inchToPixel y)

transList :: [Vertex] -> [Point]
transList = map trans


primitiveToGraphic :: Primitive -> Graphic 
primitiveToGraphic (Rectangle s1 s2) =
    let s12 = s1 / 2
        s22 = s2 / 2
    in polygon 
          (transList [(-s12, -s22), (-s12, s22), (s12, s22), (s12, -s22)])

primitiveToGraphic (Ellipse r1 r2) = 
    ellipse (trans (-r1, -r2)) (trans (r1, r2))

primitiveToGraphic (RtTriangle s1 s2) =
    polygon (transList [(0, 0), (s1, 0), (0, s2)])

primitiveToGraphic (Polygon pts) = 
    polygon (transList pts)

type ColoredPrimitives = [(Color, Primitive)]

drawPrimitives :: Window -> ColoredPrimitives -> IO ()
drawPrimitives win [] = return ()
drawPrimitives win ((clr, prim):ps) = do
    drawInWindow win (withColor clr (primitiveToGraphic prim))
    drawPrimitives win ps

shs :: ColoredPrimitives
--shs = [(Blue, Ellipse 1 1)]
shs = [(Red, Rectangle 3 2), (Blue, Ellipse 1 1.5), (Yellow, RtTriangle 3 2), (Magenta, Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2), (-1.7, -1.0), (-3.0, 0)])]

concentricCircles :: ColoredPrimitives
concentricCircles = zip [Black,Blue,Green,Cyan,Red,Magenta,Yellow,White] $ map circle [2.4,2.1..0.3]
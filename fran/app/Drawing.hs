module Drawing (Drawing (Region, Over, EmptyDrawing),
                Color (Black, Blue, Green, Cyan,
                       Red, Magenta, Yellow, White),
                regionToGRegion, primitiveToGRegion,
                drawRegionInWindow, drawDrawing, draw, spaceClose, main,
                draw2, drawing,
                module Region) where

import Viewport 
import Primitive
import Region
import GLFWWrapper hiding (Region)
import qualified GLFWWrapper as G (Region)

data Drawing = Region Color Region 
             | Drawing `Over` Drawing
             | EmptyDrawing
    deriving Show 

drawDrawing :: Window -> Drawing -> IO ()
drawDrawing w (Region c r) = drawRegionInWindow w c r
drawDrawing w (p1 `Over` p2) = do drawDrawing w p2; drawDrawing w p1
drawDrawing w EmptyDrawing = return ()

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r = drawInWindow w (withColor c (drawRegion (regionToGRegion r)))

regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0, 0) (1, 1) r

regToGReg :: Vector -> Vector -> Region -> G.Region 
type Vector = (Float, Float)
regToGReg loc sca (Primitive s) = primitiveToGRegion loc sca s
regToGReg loc (sx, sy) (Scale (u, v) r) = regToGReg loc (sx*u, sy*v) r 
regToGReg (lx, ly) (sx, sy) (Translate (u, v) r) = regToGReg (lx + u * sx, ly + v * sy) (sx, sy) r 
regToGReg loc sca Empty = createRectangle (0, 0) (0, 0)
regToGReg loc sca (r1 `Union` r2) = primGReg loc sca r1 r2 orRegion 
regToGReg loc sca (r1 `Xor` r2) = primGReg loc sca r1 r2 xorRegion
regToGReg loc sca (Complement r) = primGReg loc sca winRect r diffRegion 

primGReg loc sca r1 r2 op = let gr1 = regToGReg loc sca r1
                                gr2 = regToGReg loc sca r2 
                            in op gr1 gr2 

winRect :: Region 
winRect = Primitive (Rectangle (pixToInch winWidth) (pixToInch winHeight))

primitiveToGRegion :: Vector -> Vector -> Primitive -> G.Region 
primitiveToGRegion (lx, ly) (sx, sy) s = 
    case s of
        Rectangle s1 s2 -> createRectangle (trans (-s1/2, -s2/2))
                                           (trans (s1/2, s2/2))
        Ellipse r1 r2 -> createEllipse (trans (-r1, -r2))
                                       (trans (r1, r2))
        Polygon vs -> createPolygon (map trans vs)
        RtTriangle s1 s2 -> createPolygon (map trans [(0,0),(s1,0),(0,s2)])
    where trans :: Vertex -> Point 
          trans (x, y) = (halfWinWidth + inchToPixel (lx + x*sx), halfWinHeight - inchToPixel (ly+y*sy))

draw :: String -> Drawing -> IO ()
draw s p = runGraphics $ 
    do w <- openWindow s (winWidth, winHeight)
       drawDrawing w p
       spaceClose w 

xUnion :: Region -> Region -> Region 
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union`
                 (p2 `Intersect` Complement p1)

r1 = Primitive (Rectangle 3 2)
r2 = Primitive (Ellipse 1 1.5)
r3 = Primitive (RtTriangle 3 2)
r4 = Primitive (Polygon [(-2.5,2.5),(-3.0,0),(-1.7,-1.0),(-1.1,0.2),(-1.5,2.0)])

reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
pic1 = Region Blue reg1

reg2 = let circle = Primitive (Ellipse 0.5 0.5)
           square = Primitive (Rectangle 1 1)
       in (Scale (2,2) circle)
          `Union` (Translate (1,0) square)
          `Union` (Translate (-1,0) square)
pic2 = Region Yellow (Translate (0,-1) reg2)

pic3 = pic2 `Over` pic1
oneCircle = Primitive (Ellipse 1 1)
manyCircles = [Translate (x,0) oneCircle | x <- [0,2..]]
fiveCircles = foldr Union Empty (take 5 manyCircles)
fc = Region Red (Scale (0.25, 0.25) fiveCircles)

drawingToList :: Drawing -> [(Color, Region)]
drawingToList EmptyDrawing = []
drawingToList (Region c r) = [(c,r)]
drawingToList (p1 `Over` p2) = drawingToList p1 ++ drawingToList p2

adjust :: [(Color, Region)] -> Coordinate -> (Maybe (Color, Region), [(Color, Region)])
adjust regs p = case (break (\(_,r) -> r `containsR` p) regs) of 
                    (top, hit:rest) -> (Just hit, top++rest)
                    (_,[]) -> (Nothing, regs)

loop :: Window -> [(Color, Region)] -> IO ()
loop w regs = do clearWindow w
                 sequence_ [drawRegionInWindow w c r | (c,r) <- reverse regs]
                 (x, y) <- getLBP w
                 case (adjust regs (pixToInch (x - halfWinWidth), pixToInch (halfWinHeight - y))) of
                    (Nothing, _) -> closeWindow w 
                    (Just hit, newRegs) -> loop w (hit : newRegs)

draw2 :: String -> Drawing -> IO ()
draw2 s d = runGraphics $ 
            do w <- openWindow s (winWidth, winHeight)
               loop w (drawingToList d)

d1,d2,d3,d4 :: Drawing
d1 = Region Red r1 
d2 = Region Blue r2 
d3 = Region Green r3
d4 = Region Yellow r4

drawing :: Drawing
drawing = foldl Over EmptyDrawing [d1,d2,d3,d4]
main = draw2 "Picture Click Test" drawing

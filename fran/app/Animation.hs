module Animation where

import Primitive 
import FRP (Behaviour, Time, time, lift0, lift1, lift2, lift3, (>*), (<*)) 
import Viewport 
import Drawing 
import GLFWWrapper hiding (Region)
import qualified GLFWWrapper as G (Region)
import Prelude hiding (Time)

type Animation a = [Time] -> [a]

-- Defining the 'at' interpreter --
atAnim :: Animation a -> [Time] -> [a]
f `atAnim` t = f t

m :: Behaviour Drawing 
m = let a = lift0 (empty `over` d)
    in a `over` empty 

d :: Drawing
d = empty

main1 :: IO ()
main1 = animate "Animated Shape" (map (withColor Blue . primitiveToGraphic) . rubberBall)

main2 :: IO ()
main2 = animate "Animated Region" (map (withColor Yellow . regionToGraphic) . revolvingBall)

main3 :: IO ()
main3 = animate "Animated Picture" (map drawingToGraphic . planets)

main4 :: IO ()
main4 = animateB "Revolving Ball Behaviour" revolvingBallB

main5 :: IO ()
main5 = animateB "Flashing Ball" flashingBall 

main6 :: IO ()
main6 = animateB "Lots of Flashing Balls" revolvingBalls 

main7 :: IO ()
main7 = animateB "Bouncing rainbow basketball" bounceMulticolouredBasketball

rubberBall :: Animation Primitive
rubberBall = (\t -> [Ellipse (sin $ head t) (cos $ head t)])

revolvingBall :: Animation Region
revolvingBall t = let ball = Primitive (Ellipse 0.2 0.2)
                  in [Translate (sin $ head t, cos $ head t) ball]

planets :: Animation Drawing 
planets = (\t -> [(Region Red (Primitive $ (head $ rubberBall t))) `Over` (Region Yellow (head $ revolvingBall t))])

tellTime :: Animation String 
tellTime t = ["The time is: " ++ show t]

animate :: String -> Animation Graphic -> IO ()
animate title anim = runGraphics(
    do w <- openWindowGeneric title (Just (0,0)) (Just (winWidth, winHeight)) drawBufferedGraphic 
       t0 <- timeGetTime 
       let loop = 
            do t <- timeGetTime 
               let word32ToInt = fromInteger . toInteger 
               let ft = fromInteger (word32ToInt (t-t0)) / 1000.0
               setGraphic w (head $ anim [ft])
               spaceCloseEx w loop
       loop)

regionToGraphic :: Region -> Graphic
regionToGraphic = drawRegion . regionToGRegion 

drawingToGraphic :: Drawing -> Graphic 
drawingToGraphic (Region c r) = withColor c (regionToGraphic r)
drawingToGraphic (p1 `Over` p2) = (drawingToGraphic p1) `overGraphic` (drawingToGraphic p2)
drawingToGraphic EmptyDrawing = emptyGraphic

dt = pi / 8 :: Float 

emptyA :: Animation Drawing 
emptyA t = [EmptyDrawing]

overA :: Animation Drawing -> Animation Drawing -> Animation Drawing 
overA a1 a2 = \ts -> [(head $ a1 `atAnim` ts) `Over` (head $ a2 `atAnim` ts)]

overManyA :: [Animation Drawing] -> Animation Drawing 
overManyA = foldr overA emptyA 

animateB :: String -> Behaviour Drawing -> IO ()
animateB s pf = animate s (map drawingToGraphic . pf)

instance Eq (Behaviour a) where 
    a1 == a2 = error "Can't compare behaviours."

instance Show (Behaviour a) where 
    showsPrec n a1 = error "<< Behaviour >>"

instance Num a => Num (Behaviour a) where 
    (+) = lift2 (+)
    (*) = lift2 (*)
    negate = lift1 negate 
    abs = lift1 abs 
    signum = lift1 signum 
    fromInteger = lift0 . fromInteger 

instance Fractional a => Fractional (Behaviour a) where
    (/) = lift2 (/)
    fromRational = lift0 . fromRational 

instance Floating a => Floating (Behaviour a) where 
    pi = lift0 pi 
    sqrt = lift1 sqrt 
    exp = lift1 exp 
    log = lift1 log 
    sin = lift1 sin 
    cos = lift1 cos 
    tan = lift1 tan 
    asin = lift1 asin 
    acos = lift1 acos 
    atan = lift1 atan 
    sinh = lift1 sinh 
    cosh = lift1 cosh 
    tanh = lift1 tanh 
    asinh = lift1 asinh 
    acosh = lift1 acosh 
    atanh = lift1 atanh 

instance Combine [a] where 
    empty = [] 
    over = (++)

instance Combine (Fun a) where 
    empty = Fun id 
    Fun a `over` Fun b = Fun (a . b)

newtype Fun a = Fun (a->a)

class Combine a where 
    empty :: a 
    over :: a -> a -> a 

instance Combine Drawing where 
    empty = EmptyDrawing 
    over = Over 

instance Combine a => Combine (Behaviour a) where
    empty = lift0 empty 
    over = lift2 over 

overMany :: Combine a => [a] -> a 
overMany = foldr over empty 

reg :: Behaviour Color -> Behaviour Region -> Behaviour Drawing
reg = lift2 Region

primitive :: Behaviour Primitive -> Behaviour Region
primitive = lift1 Primitive

ell :: Behaviour Radius -> Behaviour Radius -> Behaviour Primitive
ell = lift2 Ellipse

red :: Behaviour Color 
red = lift0 Red 

yellow :: Behaviour Color 
yellow = lift0 Yellow 

translate :: (Behaviour Float, Behaviour Float) -> Behaviour Region -> Behaviour Region
translate (a1, a2) (r) = (\t -> [Translate ((head $ a1 t), (head $ a2 t)) (head $ r t)])

revolvingBallB :: Behaviour Drawing 
revolvingBallB = let ball = primitive (ell 0.2 0.2)
                 in reg red (translate (sin time, cos time) ball)

ifFun :: Bool -> a -> a -> a
ifFun p c a = if p then c else a 

cond :: Behaviour Bool -> Behaviour a -> Behaviour a -> Behaviour a
cond = lift3 ifFun 

flash :: Behaviour Color 
flash = cond (sin time >* 0) red yellow

timeTrans :: Behaviour Time -> Behaviour a -> Behaviour a 
timeTrans fb sb = \t -> sb (fb t)

flashingBall :: Behaviour Drawing 
flashingBall = let ball = primitive (ell 0.2 0.2)
               in reg (timeTrans (8*time) flash)
                      (translate (sin time, cos time) ball)

spaceCloseEx w loop 
    = do k <- maybeGetWindowEvent w 
         case k of 
            Just (Key c d) | c == ' ' && d -> closeWindow w
            Just Closed -> closeWindow w 
            Nothing -> loop 
            _ -> spaceCloseEx w loop 

revolvingBalls :: Behaviour Drawing 
revolvingBalls = overMany [ timeTrans (lift0 (t*pi/4) + time) flashingBall | t <- [0..7]]

slowTime = time 

bounceMulticolouredBasketball :: Behaviour Drawing 
bounceMulticolouredBasketball = 
    let ball = primitive (ell 0.5 0.5)
    in reg rainbowColor (translate (sideToSide, bounce) ball)

-- Cycle through a spectrum of colors
cycleColors :: [Color] -> [Time] -> [Color]
cycleColors colors times = map (\t -> colors !! (floor t `mod` length colors)) times

-- Define a behavior for the rainbow colors
rainbowColor :: Behaviour Color
rainbowColor = \times -> cycleColors spectrum times

bounce :: Behaviour Float 
bounce = abs (sin time)

-- Horizontal oscillation
sideToSide :: Behaviour Float
sideToSide times = map (sin . (pi *)) times

spectrum :: [Color]
spectrum = [c | c <- [minBound ..], c /= Black]


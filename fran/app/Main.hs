module Main where
import FRP -- Where our FRP implementation resides
import Data.Time -- For fast access to system clock
import Control.Concurrent (threadDelay)
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL (GLfloat)
import Data.IORef

import Primitive
import Viewport
import GLFWWrapper hiding (circle)
import Drawing
import Animation (main1, main2, main3, main4, main5, main6, main7, cond)

-- Utility functions

takeLastTen :: [Time] -> [Time]
takeLastTen = reverse . take 10 . reverse

checkElapsedTime :: Time -> UTCTime -> IO Bool
checkElapsedTime dur its = do
    cts <- getCurrentTime
    let et = realToFrac (diffUTCTime cts its) :: Float
    return (et >= dur)

sampleTimeForDuration :: Time -> UTCTime -> IO [Time]
sampleTimeForDuration dur its = do
    cts <- getCurrentTime
    let et = realToFrac (diffUTCTime cts its) :: Float
    if et >= dur
        then return []
        else do
            -- print cts
            threadDelay 50000
            remainingSamples <- sampleTimeForDuration dur its
            return ( et : remainingSamples)

square :: Behaviour Float
square ts = zipWith (*) ts ts

colour :: Behaviour String
colour = (lift0 "Red") `untilFRP` (when (time >* lift0 5) ==> const(lift0 "Blue"))

colour2 :: Behaviour String 
colour2 = cond (time >* (lift0 5)) (lift0 "Red") (lift0 "Blue")

bizarre :: Behaviour Float
bizarre (t0:t1:ts) = 0 : 0 : map (let c = 1 / (t1 - t0)
                                 in \t -> c*c*t*(1-t)**c) ts


loopColourFunction :: Time -> UTCTime -> [Time] -> IO ()
loopColourFunction dur its currentTimeStream = do
    cts <- getCurrentTime
    let et = realToFrac (diffUTCTime cts its) :: Time
    if et >= dur
        then return ()
        else do
            threadDelay 1000000
            putStrLn ("Time Elapsed: " ++ show et ++ "; Stream: " ++ show (currentTimeStream ++ [et]))
            putStrLn ("Time Elapsed: " ++ show et ++ "; Colour: " ++ show (colour2 (currentTimeStream ++ [et])))
            print $ ((time >* lift0 5)) (currentTimeStream ++ [et])
            loopColourFunction dur its (takeLastTen (currentTimeStream ++ [et]))


main = do
    putStrLn "Testing out simple, stream-based FRP implementation."
    -- First, let's see how constant behaviours work
    putStrLn "\n\nTest 1: Printing out a constant Behaviour"
    putStrLn ("Constant Behaviour value (should be 3): " ++ show (at (lift0 3) [1729]))

    -- -- Next, let's try to substitute this with the Time behaviour
    putStrLn "\n\nTest 2: Printing out a fixed Time Behaviour"
    putStrLn ("Constant Time Behaviour value (should be 1729): " ++ show (time [1729]))

    -- -- Next up, let's substitute the constant Time with a system time tick
    putStrLn "\n\nTest 3: Printing out the system's time"
    initialTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" initialTime
    putStrLn $ "Current UTC time: " ++ show formattedTime

    -- -- Check if one second has passed since the previous time sample
    -- threadDelay 1000000 -- artificially create the delay by stopping
    -- result <- checkElapsedTime 1 initialTime
    -- print result

    -- -- Now, for a prespecified period of time, sample time at 60fps
    initialTime <- getCurrentTime
    print initialTime
    ts <- sampleTimeForDuration 1 initialTime
    print ts
 
    -- -- Next, let's test out the `until` operator
    print ts
    print (colour ts)

    -- -- Now it's time to integrate the bizarre function (blows up)
    putStrLn ("Here's what the well-behaved function x^2 integrates to over (0, 1) (converges to 0.33 as sampling becomes more frequent): " ++ show (integral Main.square ts))
    putStrLn ("Here's what the bizarre function integrates to over (0, 1) (integral of limit = 0, limit of integral = 1): " ++ show (integral bizarre ts))


    -- Now let's implement a time loop for 15 seconds that prints out the value of a Behaviour at each sample time
    -- initialTime <- getCurrentTime
    -- loopColourFunction 15 initialTime []

    -- Trying out a Fran-esque reactive animation library
    -- runGraphics ( do w <- openWindow "Fran-esque Reactivity" (600, 600)
    --                  drawInWindow w (ellipse (150, 150) (300, 200))
    --                  spaceClose w)

    -- Trying out the viewport drawing abstractions
    -- runGraphics (
    --     do w <- openWindow "Fran-esque Reactivity: Viewport Ops" (winWidth, winHeight)
    --        drawPrimitives w shs 
    --        spaceClose w)

    -- Drawing out a bulls-eye
    -- runGraphics (
    --     do w <- openWindow "Fran-esque Reactivity: Viewport Ops" (winWidth, winHeight)
    --        drawPrimitives w concentricCircles
    --        spaceClose w)

    -- Drawing Module
    -- draw2 "Picture Click Test" drawing

    -- Animations (Finally!)
    main3

    -- Older versions: fewer abstractions, attempting to directly interface Behaviours with OpenGL code
    -- As we'll see, 
    -- (_progName, _args) <- getArgsAndInitialize
    -- _window <- createWindow "OpenGL Circle"
    -- initialDisplayMode $= [DoubleBuffered]
    -- windowSize $= Size 400 400
    -- colorRef <- newIORef (1, 0, 0) -- set to red initially
    -- streamRef <- newIORef []
    -- vertexRef <- newIORef (0.0) -- x coordinate of top vertex, to be animated as a Behaviour
    -- initialTime <- getCurrentTime
    -- displayCallback $= display colorRef vertexRef initialTime
    -- idleCallback $= Just (varyColorAndPosition colorRef vertexRef initialTime streamRef)
    -- mainLoop


-- (OLD CODE FROM PROTOTYPING)


-- Display function to draw a circle
display :: IORef(GLfloat, GLfloat, GLfloat) -> IORef (GLfloat) -> UTCTime -> DisplayCallback
display colorRef vertexRef its = do
    clear [ColorBuffer]
    cts <- getCurrentTime
    let et = realToFrac (diffUTCTime cts its) :: Time
    color <- readIORef colorRef
    topVertex <- readIORef vertexRef
    color3f color
    renderTriangle topVertex
    swapBuffers

-- Helper function to draw a circle using tris
renderTriangle :: (GLfloat) -> IO ()
renderTriangle topVertex = renderPrimitive Triangles $ do
    vertex2f (topVertex) (0.5)
    vertex2f (-0.5) (-0.5)
    vertex2f (0.5) (-0.5)

-- Time-varying color function
varyColorAndPosition :: IORef(GLfloat, GLfloat, GLfloat) -> IORef (GLfloat) -> UTCTime -> IORef [Time] -> IdleCallback
varyColorAndPosition colorRef vertexRef its streamRef = do
    cts <- getCurrentTime
    let et = realToFrac (diffUTCTime cts its) :: Time
    modifyIORef streamRef (\ts -> ts ++ [et])
    timestamps <- readIORef streamRef
    writeIORef colorRef ((last (redColor timestamps)), (last (greenColor timestamps)), 0.0)
    writeIORef vertexRef (last (redColor timestamps))
    postRedisplay Nothing

-- Utility Behaviour for red color (time-varying) 
redColor :: Behaviour Float
redColor ts = FRP.sin (time ts)

modifiedRedColor :: Behaviour Float
modifiedRedColor = lift0 1.0 `untilFRP` (when (time >* lift0 5.0) ==> const (lift0 0.0))

greenColor :: Behaviour Float
greenColor ts = FRP.cos (time ts)

modifiedGreenColor :: Behaviour Float
modifiedGreenColor = lift0 0.0 `untilFRP` (when (time >* lift0 5.0) ==> const (lift0 1.0))

-- Utility function to set RGB color
color3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
color3f (r, g, b) = color $ Color3 r g b

-- Utility for 2D vertices
vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f x y = vertex $ Vertex2 x y


module FRP where
import Graphics.Rendering.OpenGL (GLfloat)

-- Time is just an alias for Real values --
type Time = Float

-- Defining the basic constructs: Behaviours and Events --
type Behaviour a = [Time] -> [a]
type Event a = [Time] -> [Maybe a]

-- Defining the 'at' interpreter --
at :: Behaviour a -> [Time] -> [a]
f `at` t = f t

-- The simplest Behaviour: Time --
time :: Behaviour Float
time ts = ts

-- Defining stuff related to lifting --

-- 1: The 'overloaded' dollar-sign operator --
($*) :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
ff $* fb = \ts -> zipWith ($) (ff `at` ts) (fb `at` ts)

-- 2: The lift0 operator --
lift0 :: a -> Behaviour a
lift0 x = map (const x) -- mapping to an infinite stream of constant values --

-- 3: The lift1 operator --
lift1 :: (a -> b) -> (Behaviour a -> Behaviour b)
lift1 f fb = lift0 f $* fb

-- 4: The lift2 operator --
lift2 :: (a -> b -> c) -> (Behaviour a -> Behaviour b -> Behaviour c)
lift2 f fb sb = lift1 f fb $* sb

-- 5: The lift3 operator --
lift3 :: (a -> b -> c -> d) -> (Behaviour a -> Behaviour b -> Behaviour c -> Behaviour d)
lift3 f fb sb tb = lift2 f fb sb $* tb

-- The integral operator --
integral :: Behaviour Float -> Behaviour Float
integral fb =
    \ts@(t:ts') -> 0 : loop t 0 ts' (fb ts)
    where loop t0 acc (t1:ts) (a:as)
            = let acc' = acc + (t1-t0)*a
            in acc' : loop t1 acc' ts as
          loop _ acc [] _ = [acc]
          loop _ acc _ [] = [acc]

-- Event mapping --
(==>) :: Event a -> (a -> b) -> Event b
fe ==> ff = map (fmap ff) . fe

-- Choice --
(.|.) :: Event a -> Event a -> Event a
fe1 .|. fe2 =
    \ts -> zipWith aux (fe1 ts) (fe2 ts)
    where aux Nothing  Nothing  = Nothing
          aux (Just x) _        = Just x
          aux _        (Just x) = Just x

-- Behaviour Switching --
untilFRP :: Behaviour a -> Event (Behaviour a) -> Behaviour a
fb `untilFRP` fe =
    \ts -> loop ts (fe ts) (fb ts)
    where loop ts@(_:ts') ~(e:es) (b:bs) =
            b : case e of
                    Nothing  -> loop ts' es bs
                    Just fb' -> tail (fb' ts)
          loop [] _ _ = []
          loop _ [] _ = []
          loop _ _ [] = []


-- Snapshot --
snapshot :: Event a -> Behaviour b -> Event (a, b)

snapshot fe fb =
    \ts -> zipWith aux (fe ts) (fb ts)
    where aux (Just x) y = Just (x, y)
          aux Nothing  _  =  Nothing

-- Predicate Events --
when :: Behaviour Bool -> Event ()
when fb ts = zipWith up (True : bs) bs where bs = fb ts
                                             up False True = Just ()
                                             up _     _    = Nothing


-- Lifting the comparison operators
(>*), (<*) :: Ord a => Behaviour a -> Behaviour a -> Behaviour Bool
(>*) = lift2 (>)
(<*) = lift2 (<)

sin :: Behaviour Time
sin ts = map (Prelude.sin) (time ts)

cos :: Behaviour Time
cos ts = map (Prelude.cos) (time ts)

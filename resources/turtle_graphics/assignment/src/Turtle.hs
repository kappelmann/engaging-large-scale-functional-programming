module Turtle where

-- data types for Turtle
type Angle    = Float
type Distance = Float
type Turtle   = (Pen, Angle, Pnt, [Ln], [(Pnt,Angle)])        -- [Ln] contains all processed lines

-- Lines
data Ln = Ln Pen Pnt Pnt
  deriving (Eq,Ord,Show)

-- Colors
data Pen = Colour Float Float Float
         | Inkless
           deriving (Eq, Ord, Show)

white, black, red, green, blue :: Pen
white = Colour 1.0 1.0 1.0
black = Colour 0.0 0.0 0.0
red   = Colour 1.0 0.0 0.0
green = Colour 0.0 1.0 0.0
blue  = Colour 0.0 0.0 1.0

-- Points
data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x = Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

-- The last two functions are not called min and max
-- because the invariant for min and max states
-- (min x y, max x y) = (x,y) or (y,x).

polar :: Angle -> Pnt
polar ang  =  Pnt (cos radians) (sin radians)
  where
  radians  =  ang * 2 * pi / 360
  
-- Change Turtle status 
-- You can implement custom commands for apply below.
--
move :: Float -> Turtle -> Turtle
move dst t@(pen, ang, pnt, ls, bps)
  | pen == Inkless = t
  | otherwise = (pen, ang, endpnt, ls++[Ln pen pnt endpnt], bps)
  where endpnt = pnt + scalar dst * polar ang

turn :: Float -> Turtle -> Turtle
turn delta (pen, ang, pnt, ls, bps) = (pen, ang-delta, pnt, ls, bps)

branch :: Turtle -> Turtle
branch (pen, ang, pnt, ls, bps) = (pen, ang, pnt, ls, (pnt,ang):bps)

endBranch :: Turtle -> Turtle
endBranch (pen, ang, pnt, ls, []) = (pen, ang, pnt, ls, [])
endBranch (pen, ang, pnt, ls, (p,a):bps) = (pen, a, p, ls, bps)

changeColor :: Pen -> Turtle -> Turtle
changeColor new (_, ang, pnt, ls, bps) = (new, ang, pnt, ls, bps) 

sit :: Turtle -> Turtle
sit = id

module InstGen where

import FPInt
import SimMath
import Utils

type FPPos = (FPInt, FPInt)

posMap f (x1,x2) = (f x1, f x2)

-- The way we tell if an arc is similar to another if they have the same radius 
-- of curvature
data ArcR = ArcR {arc_start :: FPPos,
                  arc_radius :: Double,
                  arc_clockwise :: Bool}

similarArcs :: ArcR -> ArcR -> Bool
similarArcs a1 a2 = arc_clockwise a1 == arc_clockwise a2 && 
                    abs (arc_radius a1 - arc_radius a2) < eps
    where
      eps = 0.1 -- need to figure this out, maybe should be proportional to arc lengths
                -- so that short noisy arcs can be merged but long arcs won't be


-- Is a line an extension of this arc? Yes if the distance to the circle's center
-- equals the arc's radius
isArcExtension :: ArcR -> FPPos -> FPPos -> Bool
isArcExtension arc lstart lend = abs (square r - p3centdist2) < eps
  where
    r = arc_radius arc
    p2d = posMap fp2dbl
    asd@(x1,y1) = p2d (arc_start arc)
    aed@(x2,y2) = p2d lstart
    led@(x3,y3) = p2d lend
    d = dist asd aed
    theta = acos (d / (2 * r))
    alpha = atan ((y2 - y1) / (x2 - x1))
    gamma = if arc_clockwise arc then alpha - theta else alpha + theta
    cent = (x1 + r * cos gamma, y1 + r * sin gamma)
    p3centdist2 = dist2 cent led
    eps = 0.1 -- need to tune

data Segment = Line of FPPos
               Arc of ArcR

initSegments :: [FPPos] -> [Segment]
initSegments = map Line

-- On making arcs:
-- The arcs should already be there in the input, this is just recognizing them
-- If there are any high angle turns, adding arcs will goof it up for sure


-- Path is a sequence of points
type Path = [(FPInt,FPInt)]

segAng :: FPPos -> FPPos -> FPPos -> Double
segAng (x1,y1) (x2,y2) (x3,y3) = ang1 - ang2
    where
      ang1 = atan (fp2dbl (y2 - y1) / fp2dbl (x2 - x1))
      ang2 = atan (fp2dbl (y3 - y2) / fp2dbl (x3 - x2))

-- Calculate max exit velocities for each segment.
-- These are like speed limit signs, they signal to the generator that curves are ahead
-- Exit velocity is the min of
--  a) the max speed you can be to make the turn
--  b) the max speed you can have in order to make the following segment's exit velocity
-- What if there are impossible turns? You should replace impossible turns
exitVels :: Path -> [FPInt]
exitVels [] = [] -- eh needs arcs

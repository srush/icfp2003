-- Misc math functions related to the simulation
module SimMath where

dbl_T :: Double
dbl_T = 64 / 65536
dbl_L :: Double
dbl_L = 20000 / 65536

-- Radius of the circle whose arc you pass through if you turn ang rads inside
-- a tile
circRad :: Double -> Double
circRad ang = 1 / (2 - 2 * cos ang)

-- The fastest you can turn in rads per timestep going at v tiles per timestep
maxAng :: Double -> Double
maxAng v = dbl_T / (v * v + dbl_L)

-- The fastest you can be going to make a turn
maxVel :: Double -> Double
maxVel a = sqrt (dbl_T/a - dbl_L)

-- A turn possible in one timestep
possibleTurn :: Double -> Bool
possibleTurn a = dbl_T / a >= dbl_L

-- The amount you have to turn per timestep at velocity v along an arc of a
-- circle of radius circRad a
arcAng :: Double -> Double -> Double
arcAng v a = asin (v / (2 * circRad a))

-- Find the max velocity you can go given you need to turn a within a tile
findMaxV :: Double -> Double
findMaxV a = _findMaxV a 0 1

_findMaxV :: Double -> Double -> Double -> Double
_findMaxV ang start end = 
  if (abs diff) < eps then mid else if diff > 0 then faster else slower
  where
    eps = 0.001
    mid = (start + end) / 2
    diff = maxAng mid - arcAng mid ang
    faster = _findMaxV ang mid end
    slower = _findMaxV ang start mid 
    
maxPi4 = findMaxV (pi / 4)
maxPi2 = findMaxV (pi / 2)

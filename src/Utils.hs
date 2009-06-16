module Utils where

import qualified System.Random as R

-------------------------------------------------------------------
-- Random stuff

randRange :: (R.Random a) => a -> a -> IO a
randRange s e = R.getStdRandom (\g -> R.randomR (s,e) g)


coinFlip :: Double -> IO Bool
coinFlip rate = do
  d <- randRange 0.0 1.0
  return $ d < rate

_selectN :: [a] -> Int -> Int -> IO [a]
_selectN [] _ _ = return []
_selectN (h:t) needed avail = do
  p <- coinFlip (fromIntegral needed / fromIntegral avail)
  let needed' = if p then needed - 1 else needed 
  rest <- _selectN t needed' (avail - 1)
  return $ if p then h : rest else rest 

selectN :: [a] -> Int -> IO [a]
selectN items needed = _selectN items needed (length items)

----------------------------------------------------------------------------
-- Trig nonsense to find max velocity given that you need make a turn within
-- a tile

-- Radius of the circle whose arc you pass through if you turn ang rads inside
-- a tile
circRad :: Double -> Double
circRad ang = 1 / (2 - 2 * cos ang)

-- The fastest you can turn per timestep going at v tiles per timestep
maxAng :: Double -> Double
maxAng v = t / (v * v + l)
  where
    t = 64 / 65536
    l = 20000 / 65536
-- The amount you have to turn per timestep at velocity v along an arc of a
-- circle of radius circRad a
arcAng :: Double -> Double -> Double
arcAng v a = asin (v / (2 * circRad a))

-- Find the max velocity you can go given you need to turn a within a tile
findMaxV :: Double -> Double
findMaxV a = _findMaxV a 0 1

eps :: Double
eps = 0.001

_findMaxV :: Double -> Double -> Double -> Double
_findMaxV ang start end = 
  if (abs diff) < eps then mid else if diff > 0 then faster else slower
  where
    mid = (start + end) / 2
    diff = maxAng mid - arcAng mid ang
    faster = _findMaxV ang mid end
    slower = _findMaxV ang start mid 
    
maxPi4 = findMaxV (pi / 4)
maxPi2 = findMaxV (pi / 2)


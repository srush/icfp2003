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


-- Math
normAng :: Double -> Double
normAng a | a < -pi = normAng (a + 2 * pi)
          | a > pi = normAng (a - 2 * pi)
          | otherwise = a

square :: Num a => a -> a
square x = x * x

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt (dist2 p1 p2)

dist2 :: (Double, Double) -> (Double, Double) -> Double
dist2 (x1,y1) (x2,y2) = square (x2 - x1) + square (y2 - y1)
module Utils where

import qualified System.Random as R

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


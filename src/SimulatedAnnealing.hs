module SimulatedAnnealing where

import System.Random
import Utils

anneal :: (Ord o) => (a -> IO a) -> (a -> o) ->
          (o -> o -> Double -> Double) -> (Int -> Int -> Double) -> Int -> o ->
          a -> IO a
anneal neighbor energy p temp kmax emax init = _anneal init einit init einit 0
  where
    einit = energy init
    _anneal s e sb eb k | k >= kmax || e <= emax = return sb
                        | otherwise = do
      sn <- neighbor s
      let en = energy sn
      let (sb', eb') = if en < eb then (sn, en) else (sb, eb)
      move <- coinFlip $ p e en (temp k kmax)
      let (s', e') = if move then (sn, en) else (s, e)
      _anneal s' e' sb' eb' (k+1)

boltzmanProb :: Floating a => a -> a -> a -> a
boltzmanProb e1 e2 temp = exp (-(e2 - e1) / temp)


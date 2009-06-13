module GeneticAlgorithm where

import Car
import Utils
import Control.Monad.Reader
import qualified Data.List as L

type Genome = Trace
type Population = [Trace]

-- Default option
data GAOpts = GAOpts {mut_rate :: Double, cross_rate :: Double, 
                      tourn_size :: Int, tourn_p :: Double}
defGAOpts :: GAOpts
defGAOpts = GAOpts {mut_rate = 0.001, cross_rate = 0.07, tourn_size = 100,
                    tourn_p = 0.7}

type GenAlgM a = ReaderT GAOpts IO a

runGA :: GAOpts -> GenAlgM a -> IO a
runGA go ga = runReaderT ga go

runGADef :: GenAlgM a -> IO a
runGADef = runGA defGAOpts

mutatep :: GenAlgM Bool
mutatep = do
  GAOpts {mut_rate = m} <- ask
  lift . coinFlip $ m

mutator :: Instruction -> GenAlgM Instruction
mutator x = do
  p <- mutatep
  if p then lift randInst else return x

mutate :: Genome -> GenAlgM Genome
mutate = mapM mutator

crossoverp :: GenAlgM Bool
crossoverp = do
  GAOpts {cross_rate = c} <- ask
  lift . coinFlip $ c

_crossOver :: Genome -> Genome -> Int -> (Genome, Genome)
_crossOver p1 p2 i = (p11 ++ p21, p21 ++ p12)
  where
    (p11, p12) = splitAt i p1
    (p21, p22) = splitAt i p2

-- Not sure how to deal with traces of different lengths, ignoring
crossOver :: Genome -> Genome -> GenAlgM (Genome, Genome)
crossOver p1 p2 = do
  p <- crossoverp
  if p 
    then do
      let l = min (length p1) (length p2)
      i <- lift $ randRange 0 (l-1)
      return $ _crossOver p1 p2 i
    else return (p1, p2)

-- Smaller is better
fitness :: Genome -> Int
fitness = length

fitPairOrd :: (Genome, Int) -> (Genome, Int) -> Ordering
fitPairOrd (_, f1) (_, f2) = compare f2 f1

tournamentChoose :: Double -> Double -> Population -> IO Genome
tournamentChoose _ _ [v] = return v
tournamentChoose p pf (h:t) = do
  x <- coinFlip p
  if x then return h else tournamentChoose (p * pf) pf t

tournament :: Population -> GenAlgM Genome
tournament pop = do
  GAOpts {tourn_size = s, tourn_p = p} <- ask
  competitors <- lift $ selectN pop s
  let sortedComps = map fst $ L.sortBy fitPairOrd 
                                (map (\g -> (g, fitness g)) competitors)
  lift $ tournamentChoose p (1-p) sortedComps


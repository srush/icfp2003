module Simulation (State, init_state, crashed_state, step_car, run, convertToPath) where

import FPInt
import Car
import Test.HUnit hiding (State)
import Text.Printf
import qualified World as W
import Data.List
import Fixedmath
import qualified Debug.Trace as T

-- Simulation

-- Sim State
type State = (CarState, W.World)
    
init_state :: String -> IO State
init_state file = do
  w <- W.fromFile file
  return (initCar . W.start $ w, w)

crashed_state :: State -> Bool
crashed_state (car, w) = 
  W.isBarrier w $ car_pos car

finish_state :: State -> Bool
finish_state (car, w) = 
  W.isGoal w $ car_pos car

-- checked every time in main loop
car_pos ::CarState -> (Int, Int)
{-# INLINE car_pos #-}
car_pos  car =  (fp2int $ car_x car, fp2int $ car_y car)

convertToPath = map car_pos 

data Result = Crash Int | Finish Int | RanOut
              deriving (Eq, Show)

run ::  Trace -> W.World -> (Result, [CarState]) 
run trace world = (r, reverse path) 
    where
      (r, path) = run_one firstCar (0, [firstCar]) trace
      firstCar = initCar (W.start world) 
      run_one _ (_,path) [] = (RanOut, path)
      run_one car (i,path) (dir:rest) =
          if crashed_state (newCar,world) then 
              (Crash i, path)
          else if finish_state (newCar, world) then
              (Finish i, path)
          else run_one newCar (i+1, (newCar:path)) rest
              where newCar = step_car car dir
          

run_fast trace world = run_one firstCar 0 trace
    where
      firstCar = initCar (W.start world) 
      -- This is the tight loop of the whole program, 
      -- tail recursion and 
      run_one :: CarState -> Int -> Trace -> IO Result
      run_one _ _ [] = return RanOut
      run_one car i (dir:rest) = do 
        let newCar = step_car car dir
        let oldpos = car_pos car
        kill_car car
        if oldpos ==  car_pos newCar then
            run_one newCar (i+1) rest
         else if crashed_state (newCar,world) then return $ Crash i
         else if finish_state (newCar, world) then return $ Finish i
         else run_one newCar (i+1) rest

        
        
testSim = TestCase $ do 
  world <- W.fromFile "data/supersimple.trk"
  let trace = cycle [Acc]
  assertEqual "crash test" (Crash 80) (fst $run trace world)

testTrace = TestCase $ do 
  world <- W.fromFile "../data/example/Een.trk"
  trace <- traceFromFile "data/example/Een.trc"
  assertEqual "workd test" (Finish 100) (fst$run trace world)

simTests = TestList $ [testTrace]

--formatTrace = map (\(i, t) -> printf "T %5i : %s" (i::Int) (instToCmd t)) . zip [0..]

touch (Crash _) = 1
touch (Finish _)  = 0
touch RanOut = 0 

main = do 
  world <- W.fromFile "/home/srush/Projects/icfp2003/data/example/Een.trk"
  trace <- traceFromFile "/home/srush/Projects/icfp2003/data/example/Een.trc"
  --putStr $ unlines $ formatTrace trace
  print $ sum $ map touch $ map (\t -> run_fast t world) $ take 1000000 $ take 1000000 (cycle (tails trace))
  
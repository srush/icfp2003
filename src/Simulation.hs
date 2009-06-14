module Simulation (State, init_state, crashed_state, step_car, run) where

import FPInt
import Car
import Test.HUnit hiding (State)
import qualified World as W

-- Simulation
fac_A :: FPInt
fac_A = int2fp 24
fac_B :: FPInt
fac_B = int2fp 36
fac_T :: FPInt
fac_T = int2fp 64
fac_L :: FPInt
fac_L = int2fp 20000
fac_F0 :: FPInt
fac_F0 = int2fp 4
fac_F1 :: FPInt
fac_F1 = int2fp 12
fac_F2 :: FPInt
fac_F2 = int2fp 24

step_car :: CarState -> Instruction -> CarState
step_car (CarState {car_x = x, car_y = y, car_v = v, car_d = d}) inst =
    CarState {car_x = x', car_y = y', car_v = v'', car_d = d'}
  where
    acc = if accelp inst then fac_A else 0
    brk = if brakep inst then -fac_B else 0
    v' = abs $ v - fac_F0 + (fac_F1 `mul_fp` v) + (fac_F2 `mul_fp` (sqr_fp v)) + acc + 
         brk
    v'' = if v' < 0 then 0 else v'
    tf = fac_T `div_fp` ((sqr_fp v'') + fac_L)
    t = if turnlp inst then -tf else if turnrp inst then tf else 0
    d' = normAng_fp (d + t)
    x' = x + (v'' `mul_fp` (cos_fp d'))
    y' = y + (v'' `mul_fp` (sin_fp d'))
    

-- Sim State
type State = (CarState, W.World)
    
init_state :: String -> IO State
init_state file = do
  w <- W.fromFile file
  return (initCar . W.start $ w, w)


crashed_state :: State -> Bool
crashed_state (CarState {car_x = x, car_y = y}, w) = 
  W.isBarrier w (fp2int x, fp2int y)

finish_state :: State -> Bool
finish_state (CarState {car_x = x, car_y = y}, w) = 
  W.isGoal w (fp2int x, fp2int y)

car_pos CarState {car_x = x, car_y = y} = 
    (fp2int x, fp2int y)

data Result = Crash Int | Finish Int | RanOut
              deriving (Eq, Show)

run ::  Trace -> W.World -> (Result, [W.Pos]) 
run trace world = 
    run_one (initCar (W.start world)) (0,[]) trace
    where
      run_one _ (_,path) [] = (RanOut, path)
      run_one car (i,path) (dir:rest) =
          if crashed_state (newCar,world) then 
              (Crash i, path)
          else if finish_state (newCar, world) then
              (Finish i, path)
          else run_one newCar (i+1, (car_pos newCar):path) rest
              where newCar = step_car car dir
          

testSim = TestCase $ do 
  world <- W.fromFile "/home/srush/Projects/icfp2003/data/supersimple.trk"
  let trace = cycle [Acc]
  assertEqual "crash test" (Crash 80) (fst $run trace world)

testTrace = TestCase $ do 
  world <- W.fromFile "/home/srush/Projects/icfp2003/data/example/Een.trk"
  trace <- traceFromFile "/home/srush/Projects/icfp2003/data/example/Een.trc"
  assertEqual "workd test" (Finish 100) (fst$run trace world)


simTests = TestList $ [testTrace]
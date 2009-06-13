module Car (Instruction, accelp, brakep, turnlp, turnrp, CarState(..), 
            instToCmd) where

import FPInt

data Instruction = Roll
                 | Acc
                 | TurnL
                 | TurnR
                 | AccL
                 | AccR
                 | Brake
                 
instToCmd :: Instruction -> String
instToCmd i = case i of
  Roll -> "."
  Acc -> "a."
  TurnL -> "l."
  TurnR -> "r."
  AccL -> "al."
  AccR -> "ar."
  Brake -> "b."

accelp :: Instruction -> Bool
accelp Acc = True
accelp AccL = True
accelp AccR = True
accelp _ = False

brakep :: Instruction -> Bool
brakep Brake = True
brakep _ = False

turnlp :: Instruction -> Bool
turnlp TurnL = True
turnlp AccL = True
turnlp _ = False

turnrp :: Instruction -> Bool
turnrp TurnR = True
turnrp AccR = True
turnrp _ = False

data CarState = CarState {car_x :: FPInt, car_y :: FPInt, car_v :: FPInt, 
                          car_d :: FPInt}
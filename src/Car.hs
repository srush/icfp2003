module Car  where

import FPInt
import qualified World as W
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P 
import Control.Applicative
import Test.HUnit

data Instruction = Roll
                 | Acc
                 | TurnL
                 | TurnR
                 | AccL
                 | AccR
                 | Brake
                 deriving (Show, Eq)
type Trace = [Instruction]
                 
instToCmd :: Instruction -> String
instToCmd i = case i of
  Roll -> "."
  Acc -> "a."
  TurnL -> "l."
  TurnR -> "r."
  AccL -> "al."
  AccR -> "ar."
  Brake -> "b."

cmdToInst :: String -> Instruction
cmdToInst i = case i of
  "" -> Roll 
  "a" -> Acc
  "l"-> TurnL
  "r" -> TurnR 
  "al" -> AccL
  "la" -> AccL
  "ra" -> AccR 
  "ar" -> AccR 
  "b" -> Brake


readInst :: Parser Instruction
readInst = do
  s <- many alphaNum
  return $ cmdToInst s
 
readTrace :: Parser [Instruction] 
readTrace = sepBy readInst (char '.')

traceFromFile filename = do 
  contents <- readFile filename
  either (fail.show) (return.id)  $ parse readTrace "" contents

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
                          
initCar :: W.Pos -> CarState
initCar (x, y) = CarState (int2fp x) (int2fp y) 0 0


testSimple = TestCase $ do 
  trace <- traceFromFile "/home/srush/Projects/icfp2003/data/supersimple.trc"
  assertEqual "trace" [Acc, AccL, Roll, AccL, AccL, TurnR, Roll] trace

carTests = TestList [testSimple] 
module Car  where

import FPInt
import qualified World as W
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P 
import Control.Applicative
import Test.HUnit
import Utils
import Text.Printf
import Fixedmath 
  
type Trace = [Movement]
                 
instToCmd :: Movement -> String
instToCmd i = case i of
  Roll -> "."
  Acc -> "a."
  TurnL -> "l."
  TurnR -> "r."
  AccL -> "al."
  AccR -> "ar."
  Brake -> "b."
  
randInst :: IO Movement
randInst = do
  i <- randRange 0 6
  return $ toEnum i

cmdToInst :: String -> Movement
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


readInst :: Parser Movement
readInst = do
  s <- many alphaNum
  return $ cmdToInst s
 
readTrace :: Parser [Movement] 
readTrace = sepBy readInst (char '.')

traceFromFile filename = do 
  contents <- readFile filename
  either (fail.show) (return.id)  $ parse readTrace "" contents

                          
initCar :: W.Pos -> CarState
initCar (x, y) = make_car (int2fp x) (int2fp y) 0 0

formatCar :: CarState -> String
formatCar car = printf "%10i%10i%10i%10i%10i" (car_x car) (car_y car) (car_v car) (car_d car) (car_d car) 

formatPath :: [CarState] -> [String]
formatPath cars = map (\(i,car) -> printf "%4i :%s" (i::Int) (formatCar car)::String) $  zip [0..] cars  

testSimple = TestCase $ do 
  trace <- traceFromFile "/home/srush/Projects/icfp2003/data/supersimple.trc"
  assertEqual "trace" [Acc, AccL, Roll, AccL, AccL, TurnR, Roll] trace

testFormat = TestCase $ do 
               print $ formatPath  [make_car 23265338
                                             21102591
                                             40
                                             (-209)]

carTests = TestList [testFormat] 
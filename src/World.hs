module World where 

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P 
import Control.Applicative
import Data.Array
import Test.HUnit
import Debug.Trace
import Control.Monad (liftM)

type Pos = (Int, Int) 
data World = World {
      ground :: Array Int (Array Int Square),
      start :: Pos, 
      height :: Int,
      width :: Int
} deriving Show

-- Road True if it is finish
data Square = Road Bool | Wall 
              deriving Show


getSquare ::  World -> Pos -> Square 
getSquare world (x,y) = ((ground world) ! y) ! x

isRoad :: World -> Pos -> Bool
isRoad world pos  = 
    case getSquare world pos of
      Road _ -> True
      _ -> False

isBarrier :: World -> Pos -> Bool
isBarrier world pos =  
    case getSquare world pos of
      Road _ -> False
      _ -> True


isGoal :: World -> Pos -> Bool
isGoal world pos  = 
    case getSquare world pos of
      Road a -> a
      _ -> False


lexer = P.makeTokenParser emptyDef
integer = P.integer lexer

eol :: Parser Char
eol = char '\n'

fromFile :: String -> IO World
fromFile filename = do
  contents <- readFile filename
  either (fail.show) (return.id)  $ parse readWorld "" contents

readSquare :: Parser Square
readSquare = 
  Wall <$ oneOf "grwb" <|>
  Road False <$ char '.' <|>
  Road True  <$ char '!' <|>
  Road False  <$ char '*' 
  

readWorld :: Parser World
readWorld = do
  w <- fromInteger `liftM` integer
  h <- fromInteger `liftM` integer
  grid <- sepBy (many readSquare) eol
  let innerArrays = map (listArray (0,  w-1)) grid 
  let arrayForm =  listArray (0, h-1) innerArrays
  return $ World arrayForm (0,0) h w

testSimple = TestCase $ do 
  world <- fromFile "/home/srush/Projects/icfp2003/data/supersimple.trk"
  assertEqual "road fail" False $ isRoad world (0,0)
  assertEqual "barrier check" True $ isBarrier world (0,0)
  assertEqual "road success" True $ isRoad world (1,1) 
  assertEqual "goal" True $ isGoal world (1,2) 

worldTests = TestList $ [testSimple]
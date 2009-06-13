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

-- Road True False if it is finish and not start
data Square = Road Bool Bool | Wall 
              deriving Show


getSquare ::  World -> Pos -> Square 
getSquare world (x,y) = ((ground world) ! y) ! x

isRoad :: World -> Pos -> Bool
isRoad world pos  = 
    case getSquare world pos of
      Road _ _-> True
      _ -> False

isBarrier :: World -> Pos -> Bool
isBarrier world pos =  
    case getSquare world pos of
      Road _ _ -> False
      _ -> True


isGoal :: World -> Pos -> Bool
isGoal world pos  = 
    case getSquare world pos of
      Road a _-> a
      _ -> False


lexer = P.makeTokenParser emptyDef
integer = P.integer lexer

eol :: Parser Char
eol = char '\n'

fromFile :: String -> IO World
fromFile filename = do
  contents <- readFile filename
  either (fail.show) (return.id)  $ parse readWorld "" contents

-- oh god, so lazy.
findStart :: Array Int (Array Int Square) -> Pos
findStart grid = maybe (0,0) id $ foldr findRow Nothing (assocs grid) 
    where findRow (i,v) Nothing = foldr (findSquare i) Nothing (assocs v) 
          findRow _ s = s 
          findSquare i (j, Road _ True) Nothing = Just (j,i)
          findSquare _ _ s = s

readSquare :: Parser Square
readSquare = 
  Wall <$ oneOf "grwb" <|>
  Road False False <$ char '.' <|>
  Road True False <$ char '!' <|>
  Road False True <$ char '*' 
  

readWorld :: Parser World
readWorld = do
  w <- fromInteger `liftM` integer
  h <- fromInteger `liftM` integer
  grid <- sepBy (many readSquare) eol
  let innerArrays = map (listArray (0,  w-1)) grid 
  let arrayForm =  listArray (0, h-1) innerArrays
  return $ World arrayForm (findStart arrayForm) h w

testSimple = TestCase $ do 
  world <- fromFile "/home/srush/Projects/icfp2003/data/supersimple.trk"
  assertEqual "road fail" False $ isRoad world (0,0)
  assertEqual "barrier check" True $ isBarrier world (0,0)
  assertEqual "road success" True $ isRoad world (1,1) 
  assertEqual "goal" True $ isGoal world (1,2) 
  assertEqual "start" (2,3) $ start world

worldTests = TestList $ [testSimple]
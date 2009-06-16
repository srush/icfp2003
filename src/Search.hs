module Search where 

import World
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Data.Maybe
import Data.Function
import Test.HUnit hiding (State)
import Debug.Trace
import Data.Bits
type Exit = Double
data Speed = Slow | Medium | Fast | SuperFast deriving (Enum, Ord, Eq, Show)


data Distance = Distance {backtrace:: ![Pos],
                          soFar :: !Double} 
              deriving (Show)

instance Eq Distance where
    (==) = (==) `on` soFar

instance Ord Distance where
    compare = compare `on` soFar

initDistance = Distance [] 0.0
maxDistance = Distance [] 10000000.0

type CellKey = (Pos, Exit, Speed)
data DPState = DPState {
      world :: World,
      table :: M.Map CellKey Distance,
      count :: !Int 
} deriving (Show)

--convertConstant = 3
--convertToGrid (x,y) =  (x `shiftR` convertConstant, y `shiftR` convertConstant)
--convertFromGrid (x,y) =  (x `shiftL` convertConstant, y `shiftL` convertConstant)

convertConstant = 2^3 +3 
convertToGrid (x,y) =  (x `div` convertConstant, y `div` convertConstant)
convertFromGrid (x,y) =  (x * convertConstant, y * convertConstant)

type MemoState = State DPState
cartesian as bs = [(a,b) | a<-as, b <-bs]

angle = pi / 16
allDir = map (angle*) [0..31]

allSpeed = [SuperFast, Fast, Medium, Slow]

allMovements = cartesian allDir allSpeed

toOffset :: Double -> (Int, Int)
toOffset angle = ( round (2*(cos angle)) , -round (2*(sin angle))) 

onlyWest = filter pred allDir
    where pred x = (fst $ toOffset x) < 0  

notWest = filter pred allDir
    where pred x = (fst $ toOffset x) >= 0  


addValue :: CellKey -> Distance -> MemoState ()
addValue key value = do 
  dpstate <- get 
  put $ dpstate{ table = M.insert key value (table dpstate)}

removeKey :: CellKey -> MemoState ()
removeKey key  = do 
  dpstate <- get 
  put $ dpstate{ table = M.delete key (table dpstate)}

incCount :: MemoState ()
incCount = do 
  dpstate <- get
  put $ (if (count dpstate) `mod` 1000 == 0 then trace (show $count dpstate) else id) $ dpstate { count = count dpstate + 1}

getValue :: CellKey -> MemoState (Maybe Distance)
getValue key = do 
  dpstate <- get 
  return $ M.lookup key (table dpstate)

initializeState :: Pos -> [Pos] -> MemoState ()
initializeState beginPos goals = do 
  mapM_ (\(x,y) -> mapM_ (\(dir,speed) -> addValue ((x,y), dir, speed) maxDistance) $ cartesian notWest allSpeed) goals 
  mapM_ (\(dir,speed) -> addValue (beginPos, dir, speed) initDistance) $  allMovements

startDP :: [Pos] -> MemoState [[Distance]] 
startDP goals  =
    mapM (\(x,y) -> mapM (\(dir, speed) -> shortestAll ((x,y), dir, speed)) $ cartesian onlyWest allSpeed) goals

shortestPath ::  World -> Distance
shortestPath world = minimum $ filter (\m -> (length $ backtrace m) > 50 ) $ concat $ res
    where
      begin = convertToGrid $ trace (show $ start world) start world 
      goals = filter (begin /=) $ concat $ map (\(x,y) ->  [(x+xoff,y+yoff) | xoff <- [-4..2], yoff <- [-2..2]]) $ map convertToGrid $ findGoals $ ground world
      
      (res,_) = runState (initializeState begin goals >> startDP goals)  (DPState world M.empty 0) 

turnCost SuperFast  = 1.0
turnCost Fast  = 2.0
turnCost Medium  = 5.0
turnCost Slow  =  8.0

constructPossibilities (pos,toDir,toSpeed) (fromDir,fromSpeed) = do
  let (x,y) = (toOffset fromDir) :: (Int,Int)
  let (x',y') = pos
  let mov = (sqrt ((fromIntegral x)**2 + (fromIntegral y)**2))
  val <- shortestAll ((x+x', y+y'), fromDir, fromSpeed) -- $ trace ("At "++(show pos)++ " checking " ++ (show (x+x',y+y')) ++ " to dir "++ (show toDir) ++ " from dir " ++ (show fromDir)) fromDir
  return  $ Distance {soFar = soFar val + ((turnCost fromSpeed)  *  mov),
                      backtrace =  (convertFromGrid pos) : backtrace val}



shortestAll :: CellKey ->  MemoState Distance 
shortestAll key@(p, toDir, toSpeed) = do
  st <- get
  if any (isBarrier (world st)) aroundFromGrid then 
      return maxDistance
   else case M.lookup key (table st) of 
    Just v -> return  v 
    Nothing -> do
          incCount
          shortestTo key                     
            --trace (show $table st)$ trace (show (p,toDir)) $ mapM (shortestTo p) $ blockTurnBack toDir
            -- mapM removeKey $ map (addPos p) allDir
            --
            --let Just v  = M.lookup (p,toDir) (table st)
            --return $ v
    where (x,y) = convertFromGrid p
          aroundFromGrid = [(x+xoff,y+yoff) | xoff <- [0..10], yoff <- [0..10]]

data Turn = Sharp 
          | Slight
          | Straight
          | TooSharp
            deriving Show

--computeTurn  next prev
computeTurn a b = 
    if diff' < (2*angle) then Straight
    else if diff' < (4*angle) then Slight
    else if diff' < (6*angle) then Sharp 
    else TooSharp
        where 
          diff = abs (a - b)
          diff'= if diff > pi then (2*pi) - diff else diff
-- validMove next prev 
validMove (d',s') (d,s) =
    case  (s, s', turn) of -- trace (show turn ++ " " ++ show (d,d'))
      (SuperFast,   SuperFast,    Straight) -> True 
      (SuperFast,   Fast,    Straight) -> True 
      (Fast,   Fast,    Straight) -> True
      (Medium, Fast,    Straight) -> True
      (Slow,   Medium,  Straight) -> True
      (Slow,   Slow,    Straight) -> True
      (Medium, Slow,    Straight) -> True
      (Fast,   Slow,    Straight) -> True
      (Fast,   Medium,  Straight) -> True
      
      (Fast,   Fast,  Slight)   -> True
      (Fast,   Medium,  Slight)   -> True
      (Medium, Medium,  Slight)   -> True
      (Slow,   Slow,    Slight)   -> True
      (Medium, Slow,    Slight)   -> True

      (Medium, Slow,    Sharp)   -> True
      (Slow, Slow,    Sharp)   -> True
      
      --(Slow,    Slow,    Sharp)    -> True
      _ -> False
    where turn = computeTurn d d'

shortestTo :: CellKey -> MemoState Distance 
shortestTo key@(pos, toDir, toSpeed) = do
  st <-  get 
  addValue key maxDistance -- lock this space 
  let speeddirs =  filter (validMove (toDir, toSpeed)) $ allMovements
  vals <- mapM (constructPossibilities key) speeddirs -- $ trace (show dirs) dirs 
  let m = mymin vals
  addValue key m -- $  trace (show (pos,toDir, m)) $ m
  return m -- $ trace "returning" m
      where mymin = foldl (\ a b -> if a < b then a else b) maxDistance


testCurve = TestCase $ do 
  world <- fromFile "/home/srush/Projects/icfp2003/data/curve.trk"
  print $ show $ shortestPath world
  print "hello"

testSimpleCurve = TestCase $ do 
  world <- fromFile "/home/srush/Projects/icfp2003/data/1_Simple.trk"
  print $ show $ shortestPath world
  

searchTests = TestList $ [testSimpleCurve]

--main = do 
--  world <- fromFile "/home/srush/Projects/icfp2003/data/1_Simple.trk"
--  print $ show $ shortestPath world

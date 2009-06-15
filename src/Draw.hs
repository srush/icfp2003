module Main where 

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import World
import Car
import Simulation
import Data.Array
import Data.Monoid
import Control.Concurrent
import Search

resX = 1000
resY = 1000

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()


main :: IO ()
main = do
    world <- fromFile "/home/srush/Projects/icfp2003/data/1_Simple.trk"
    let path = shortestPath world 
    --    f <- readFile "/home/srush/Projects/icfp2003/src/dp.trace"
    --trace <- traceFromFile "/home/srush/Projects/icfp2003/data/example/Een.trc" 
    --let (result, cars) = run trace world
    --let path = convertToPath cars
    -- let path =  read f
    print path
    print "Starting to draw"
    initScreen

    SDL.glSwapBuffers

    Draw.draw $ Draw.scale 0.0008 0.0008  $ Draw.translate (fromIntegral (width world) / (-1.0), fromIntegral (height world)) $  
                mconcat [(drawPath $ backtrace path), (drawLinePath $ backtrace path), (drawWorld world)]  
    
    SDL.glSwapBuffers
    waitClicks
 
    SDL.waitEvent
    
    SDL.quit
    return ()

    where
    waitClicks = do
        ev <- SDL.waitEvent
        case ev of
             SDL.Quit -> return ()
             _ -> waitClicks


square = Draw.scale (sqrt 2) (sqrt 2) $ Draw.rotate (pi / 4.0) $ Draw.regularPoly 4

transToWorld :: (Int, Int) -> Draw.Draw () -> Draw.Draw ()
transToWorld (x, y) = Draw.translate (fromIntegral (2*x), fromIntegral (-2*y))

drawSquare (Road False _) = Draw.color (255,255,255,255) square 
drawSquare (Road True _) = Draw.color (50,50,0,255) square 
drawSquare (Wall)    = Draw.empty 

drawRow :: Array Int Square -> Draw.Draw () 
drawRow row = foldr (\(i, v) -> mappend (transToWorld (i,0) $ drawSquare v)) Draw.empty (assocs row) 

drawWorld :: World -> Draw.Draw () 
drawWorld world = 
    foldr (\(i, v) -> mappend (transToWorld (0, i) $ drawRow v)) 
          Draw.empty (assocs (ground world)) 

drawPath :: [(Int, Int)] -> Draw.Draw()
drawPath = mconcat . 
           map (\pos -> transToWorld pos $ Draw.color (0,0,255,255) square)  

drawLinePath :: [(Int, Int)] -> Draw.Draw()
drawLinePath path = mconcat . 
               map (\((x,y),(x2,y2)) -> transToWorld ((x+x2) `div` 2,(y+y2) `div`2) $ Draw.color (0,0,255,255) $ Draw.line (0,0) (fromIntegral (x-x2),fromIntegral(y2-y))) $ 
                   zip path $ tail path
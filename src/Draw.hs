module Main where 

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import World
import Data.Array
import Data.Monoid
--import System.Posix

resX = 600
resY = 600

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()


main :: IO ()
main = do
    initScreen

    SDL.glSwapBuffers
    world <- fromFile "/home/srush/Projects/icfp2003/data/1_Simple.trk"
    Draw.draw $ Draw.scale 0.0005 0.0005  $ drawWorld world
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

drawSquare (Road False ) = Draw.color (0,0,0,255) square 
drawSquare (Road True ) = Draw.color (10,10,10,255) square 
drawSquare (Wall)    = Draw.color (0,255,0,0) square 

drawRow row = foldr (\(i, v) -> mappend (Draw.translate (fromIntegral (2*i),0) (drawSquare v))) Draw.empty (assocs row) 

drawWorld :: World -> Draw.Draw () 
drawWorld world = Draw.translate (fromIntegral (width world) / (-2.0), fromIntegral (height world) / 2.0) $ 
    foldr (\(i, v) -> mappend (Draw.translate (0, fromIntegral (-2*i)) (drawRow v))) 
          Draw.empty (assocs (ground world)) 
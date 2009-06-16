module ShortestPath where

import qualified World as W
import qualified Data.Map as M
import qualified Data.List as L

data TileType = Wall
              | Goal
              | Road
                deriving (Show, Eq)

squareToTile :: W.Square -> TileType
squareToTile s = case s of
                   W.Road True _ -> Goal
                   W.Road _ _ -> Road
                   W.Wall -> Wall

combTile :: TileType -> TileType -> TileType
combTile Goal _ = Goal
combTile _ Goal = Goal
combTile Wall _ = Wall
combTile _ Wall = Wall
combTile Road Road = Road

coarseView :: W.World -> Int -> Int -> Int -> TileType
coarseView w gran x y = scanTiles
    where
      tl = (x * gran, y * gran)
      scanTiles = foldl combTile Road $
                  concat [[squareToTile $ W.getSquare w (i,j) |
                           i <- [max 0 x..min (W.width w) x+gran-1]] |
                          j <- [max 0 y..min (W.height w) y+gran-1]]


data CoarseWorld = CoarseWorld {cw_get :: Int -> Int -> TileType,
                                cw_height :: Int,
                                cw_width :: Int,
                                cw_gran :: Int,
                                cw_start :: (Int, Int)}

makeCoarseWorld :: W.World -> Int -> CoarseWorld
makeCoarseWorld w gran = CoarseWorld {cw_get = coarseView w gran,
                                      cw_height = W.height w `div` gran,
                                      cw_width = W.width w `div` gran,
                                      cw_gran = gran,
                                      cw_start = let (x,y) = W.start w in
                                                (x `div` gran, y `div` gran)}

cw_neighbors (x,y) w =
    filter (\(x,y) -> x >= 0 && y >= 0 &&
            x < cw_width w && y < cw_height w &&
            cw_get w x y == Road)
               [(x+1,y),(x-1,y),(y+1,x),(y-1,x)]

-- Hurr
bfsSp :: CoarseWorld -> (Int, Int) -> M.Map (Int, Int) Int
bfsSp w p = _bfsSp 1 1 [p] (M.singleton p 0)
  where
    neibs (x,y) m = filter (\(x,y) -> x >= 0 && y >= 0 &&
                            x < cw_width w && y < cw_height w &&
                           not (M.member (x,y) m) &&
                           cw_get w x y == Road)
                    [(x+dx, y+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
    _bfsSp _ _ [] done = done
    _bfsSp depth 0 q d = _bfsSp (depth+1) (length q) q d
    _bfsSp depth lleft (h:t) done =
        let ns = neibs h done in
        _bfsSp depth (lleft - 1) (t ++ ns)
         (foldl (\m np -> M.insert np depth m) done ns)

buildPathTo :: (Int,Int) -> (Int,Int) -> M.Map (Int, Int) Int -> [(Int,Int)]
buildPathTo st ed pds = _buildPathTo ed []
    where
      _buildPathTo cp@(x,y) acc
          | cp == st = st:acc
          | otherwise =
              let np = L.minimumBy (\p1 p2 ->
                                        case M.lookup p1 pds of
                                          Just v -> case M.lookup p2 pds of
                                                      Just v2 -> v `compare` v2
                                                      Nothing -> LT
                                          Nothing -> GT)
                       [(x+dx, y+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
              in _buildPathTo np (cp:acc)

test c = do
  w <- W.fromFile "../data/Een.trk"
  print "loaded"
  let cw = makeCoarseWorld w c
  let m = bfsSp cw (cw_start cw)
  let p = buildPathTo (cw_start cw) (484 `div` c,984 `div` c) m
  print p

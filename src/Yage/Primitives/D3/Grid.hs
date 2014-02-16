module Yage.Primitives.D3.Grid where

import Yage.Prelude hiding (Index)

import Data.List (drop, tail)

import Yage.Math

import Yage.Primitives.D3.Basic


-- | creates a grid along the xz plane with its center in origin and 1/1 dimension
-- divisions along x and y. for a propper positioning of the center in origin, divisions
-- sould be even
-- TODO offset left
grid :: (Floating v, Enum v) => V2 Int -> V2 v -> Primitive (V3 v)
grid divs@(V2 xdiv zdiv) dim
  | xdiv < 1 || zdiv < 1 = error "invalid divisions"
  | otherwise = 
    let V2 xStep zStep   = 1.0 / (fromIntegral <$> divs)
        verts            = genVerts xStep zStep (dim / (-2.0))
        nextrow          = drop xdiv verts
        faces            = [ Face a b c d | a <- verts, b <- nextrow, c <- tail nextrow, d <- tail verts ]
    in Grid $ faces
  where
    genVerts :: (Floating v, Enum v) => v -> v -> V2 v -> [V3 v]
    genVerts xStep zStep (V2 left back) =
          [ V3 (left + x * xStep) 0.0 (back + z * zStep)  
          | z <- [ 0.0 .. fromIntegral zdiv ]
          , x <- [ 0.0 .. fromIntegral xdiv ]
          ] -- x runs first

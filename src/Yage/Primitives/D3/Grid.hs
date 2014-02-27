{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PackageImports #-}
module Yage.Primitives.D3.Grid where

import Yage.Prelude hiding (Index, init, tail)

import Yage.Data.List (init, tail, chunksOf)

import Yage.Math

import Yage.Primitives.D3.Basic
import Yage.Geometry.Vertex
import Yage.Geometry.Elements


-- | creates a grid along the xz plane with its center in origin and 1/1 dimension
-- divisions along x and y. for a propper positioning of the center in origin, divisions
-- sould be even
-- TODO offset left
grid :: (Floating a, Enum a) => V2 Int -> V2 a -> Primitive (Vertex (P3 pn a))
grid divs@(V2 xdiv zdiv) dim
  | xdiv < 1 || zdiv < 1 = error "invalid divisions"
  | otherwise = 
    let V2 xStep zStep   = 1.0 / (fromIntegral <$> divs)
        verts            = genVerts xStep zStep (dim / (-2.0))
        rows             = chunksOf (xdiv+1) verts
        faces            = concat $ [ rowFaces r n | r <- init rows | n <- tail rows ]
    in Grid $ faces
  where
    rowFaces row nextrow = [ (position3 =:) <$> Face a b c d | a <- init row | b <- init nextrow | c <- tail nextrow | d <- tail row ]
    genVerts :: (Floating v, Enum v) => v -> v -> V2 v -> [V3 v]
    genVerts xStep zStep (V2 left back) = 
          [ V3 (left + (fromIntegral x) * xStep) 0.0 (back + (fromIntegral z) * zStep)  
          | z <- [ 0 .. zdiv ]
          , x <- [ 0 .. xdiv ]
          ] -- x runs first

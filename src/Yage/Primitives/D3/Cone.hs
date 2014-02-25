module Yage.Primitives.D3.Cone where

import Yage.Prelude

import Yage.Data.List (shift, init, zip)
import Yage.Math hiding (height)

import Yage.Primitives.D3.Basic
import Yage.Geometry.Vertex
import Yage.Geometry.Elements

cone :: (Floating a, Enum a) => Float -> Float -> Int -> Primitive (Vertex (P3 pn a))
cone radius height divs =
    let h           = realToFrac height
        r           = realToFrac radius
        d           = realToFrac divs
        tip         = V3 0 h 0
        baseCenter  = V3 0 0 0
        basev       = [ V3 (r * cos a ) 0 (r * sin a) | a <- init [0, 2 * pi / d .. 2 * pi ] ]
        mantle      = [ (position3 =:) <$> Triangle tip a b        | (a, b) <- zip basev (shift basev) ]
        base        = [ (position3 =:) <$> Triangle baseCenter a b | (a, b) <- zip (shift basev) basev ] 
    in Cone mantle base


--coneMesh :: Float -> Float -> Int -> MeshData Vertex3P3N
--coneMesh radius height divs = trianglesToMesh (cone radius height divs) (normalCalculator FacetteNormals)
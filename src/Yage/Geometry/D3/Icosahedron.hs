module Yage.Geometry.D3.Icosahedron where

import Yage.Prelude
import Yage.Math

import Yage.Data.List (shift, init, transpose)

import Yage.Geometry.D3.Basic
import Yage.Geometry.Vertex
import Yage.Geometry.Elements



icosahedron :: (Floating a, Enum a) => Float -> Primitive (Vertex (P3 pn a))
icosahedron radius = Icosahedron top middle bottom 
    where r             = realToFrac radius
          north         = V3 0   r  0
          south         = V3 0 (-r) 0
          topv          = [ V3 (r * cos a * sin theta) (  r  * cos theta) (r * sin a * sin theta)   | a <- init [ 0     , 2 * pi / 5 .. 2 * pi ] ]
          botv          = [ V3 (r * cos a * sin theta) ((-r) * cos theta) (r * sin a * sin theta)   | a <- init [ pi / 5, 3 * pi / 5 .. 2 * pi + pi / 5 ] ]
          V3 _ y _      = signorm $ V3 0 1 ((1 + sqrt 5)/2)
          theta         = acos y
          top           = [ (position3 =:) <$> Triangle north a b | (a, b) <- zip topv (shift topv) ]
          bottom        = [ (position3 =:) <$> Triangle south a b | (a, b) <- zip (shift botv) (botv) ]
          middleTop     = zipWith3 Triangle (shift topv) topv (shift botv)
          middleBottom  = zipWith3 Triangle botv (shift botv) (topv)
          middle        = concat $ transpose [map ((position3 =:) <$>) middleTop, map ((position3 =:) <$>) middleBottom]


--icosahedronMesh :: NormalSmoothness -> Float -> MeshData Vertex3P3N
--icosahedronMesh smoothness = flip trianglesToMesh (normalCalculator smoothness) . icosahedron

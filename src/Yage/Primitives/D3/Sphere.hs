module Yage.Primitives.D3.Sphere where

import Yage.Prelude
import Yage.Math

import Yage.Primitives.D3.Basic
import Yage.Primitives.D3.Icosahedron
import Yage.Geometry.Vertex
import Yage.Geometry.Elements

geoSphere :: (Floating a, Enum a, Epsilon a) 
          => Int -> Float -> Primitive (Vertex (P3 pn a))
geoSphere iter radius =
    let Icosahedron top mid bot = icosahedron radius
    in GeoSphere $ triangulate iter position3 $ top ++ mid ++ bot
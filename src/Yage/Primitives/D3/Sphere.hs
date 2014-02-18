module Yage.Primitives.D3.Sphere where

import Yage.Prelude
import Yage.Math

import Data.List

import Yage.Vertex
import Yage.Primitives.D3.Basic
import Yage.Primitives.D3.Icosahedron


geoSphere :: (Floating a, Enum a, Epsilon a) 
          => Int -> Float -> Primitive (Vertex (P3 pn a))
geoSphere iter radius =
    let Icosahedron top mid bot = icosahedron radius
    in GeoSphere $ triangulate iter position3 $ top ++ mid ++ bot
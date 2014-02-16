module Yage.Primitives.D3.Sphere where

import Yage.Prelude
import Yage.Math

import Data.List

import Yage.Primitives.D3.Basic
import Yage.Primitives.D3.Icosahedron


geoSphere :: (Floating v, Enum v, Epsilon v) 
          => Int -> Float -> Primitive (V3 v)
geoSphere iter radius =
    let Icosahedron top mid bot = icosahedron radius
    in GeoSphere $ triangulate iter $ top ++ mid ++ bot
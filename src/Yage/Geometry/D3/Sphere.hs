{-# LANGUAGE TemplateHaskell #-}
module Yage.Geometry.D3.Sphere where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry.D3.Icosahedron
import Yage.Geometry.Elements

-------------------------------------------------------------------------------

data GeoSphere v = GeoSphere { _geoSphereTris :: [Triangle v] }
    deriving ( Show, Functor, Foldable, Traversable , Generic)

makeLenses ''GeoSphere

geoSphere :: (Floating a, Enum a, Epsilon a) 
          => Int -> Double -> GeoSphere (V3 a)
geoSphere iter radius =
    let Icosahedron top mid bot = icosahedron radius
    in GeoSphere $ triangulate iter $ top ++ mid ++ bot
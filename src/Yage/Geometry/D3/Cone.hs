{-# LANGUAGE TemplateHaskell #-}

module Yage.Geometry.D3.Cone where

import Yage.Prelude
import Yage.Lens

import Yage.Data.List (shift, init)
import Yage.Math

import Yage.Geometry.Elements

data Cone v = Cone
    { _coneMantle    :: [Triangle v]
    , _coneBase      :: [Triangle v]
    } deriving ( Show, Functor, Foldable, Traversable, Generic )

makeLenses ''Cone
    


cone :: (Floating a, Enum a) => Double -> Double -> Int -> Cone (V3 a)
cone radius height divs =
    let h           = realToFrac height
        r           = realToFrac radius
        d           = realToFrac divs
        tip         = V3 0 h 0
        baseCenter  = V3 0 0 0
        basev       = [ V3 (r * cos a ) 0 (r * sin a) | a <- init [0, 2 * pi / d .. 2 * pi ] ]
        mantle      = [ Triangle tip a b        | (a, b) <- zip basev (shift basev) ]
        base        = [ Triangle baseCenter a b | (a, b) <- zip (shift basev) basev ] 
    in Cone mantle base


--coneMesh :: Float -> Float -> Int -> MeshData Vertex3P3N
--coneMesh radius height divs = trianglesToMesh (cone radius height divs) (normalCalculator FacetteNormals)
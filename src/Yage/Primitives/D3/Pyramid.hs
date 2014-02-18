module Yage.Primitives.D3.Pyramid where

import Yage.Prelude

import Data.List
import Yage.Math

import Yage.Primitives.D3.Basic
import Yage.Vertex


pyramid :: (Floating a, Enum a, Real a) => V3 a -> Primitive (Vertex (P3 pn a))
pyramid dim = 
    let (V3 x h z)  = (realToFrac <$> dim) / V3 2 1 2
        tip         = V3 0 h 0
        basev       = [ V3 (-x) 0 z, V3 x 0 z, V3 x 0 (-z), V3 (-x) 0 (-z) ]
        mantle      = [ (position3 =:) <$> Triangle tip a b  | (a, b)    <- zip (shift basev) basev ]
        base        = [ (position3 =:) <$> Triangle a b c    | (a, b, c) <- zip3 basev (shift basev) (shift $ shift basev) ] 
    in Pyramid mantle base

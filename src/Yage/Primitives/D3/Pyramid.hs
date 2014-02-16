module Yage.Primitives.D3.Pyramid where

import Yage.Prelude

import Data.List
import Yage.Math

import Yage.Primitives.D3.Basic


pyramid :: (Floating v, Enum v, Real a) => V3 a -> Primitive (V3 v)
pyramid dim = 
    let (V3 x h z)  = (realToFrac <$> dim) / V3 2 1 2
        tip         = V3 0 h 0
        basev       = [ V3 (-x) 0 z, V3 x 0 z, V3 x 0 (-z), V3 (-x) 0 (-z) ]
        mantle      = [ Triangle tip a b  | (a, b)    <- zip (shift basev) basev ]
        base        = [ Triangle a b c    | (a, b, c) <- zip3 basev (shift basev) (shift $ shift basev) ] 
    in Pyramid mantle base

module Yage.Primitives.D3.Quad where

import Yage.Prelude

import Linear
import Yage.Primitives.D3.Basic

---------------------------------------------------------------------------------------------------
-- Primitives

quad :: Fractional v => V2 v -> Primitive (V3 v)
quad dim = 
    let V2 x y = dim / 2.0
        tl     = V3 (-x)   y  0.0
        tr     = V3   x    y  0.0
        br     = V3   x  (-y) 0.0
        bl     = V3 (-x) (-y) 0.0
    in Quad $ Face tl bl br tr


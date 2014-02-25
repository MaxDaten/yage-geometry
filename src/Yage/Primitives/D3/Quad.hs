module Yage.Primitives.D3.Quad where

import Yage.Prelude

import Linear
import Yage.Primitives.D3.Basic
import Yage.Geometry.Vertex
import Yage.Geometry.Elements

---------------------------------------------------------------------------------------------------
-- Primitives

quad :: (Floating a) => V2 a -> Primitive (Vertex (P3 pn a))
quad dim = 
    let V2 x y = dim / 2.0
        tl     = position3 =: V3 (-x)   y  0.0
        tr     = position3 =: V3   x    y  0.0
        br     = position3 =: V3   x  (-y) 0.0
        bl     = position3 =: V3 (-x) (-y) 0.0
    in Quad $ Face tl bl br tr


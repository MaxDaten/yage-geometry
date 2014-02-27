{-# LANGUAGE RecordWildCards #-}
module Yage.Primitives.D3.Cube where

import Yage.Prelude hiding (Index)
import Yage.Lens

import Linear (V3(..), _x, _y, _z)
import Yage.Primitives.D3.Basic
import Yage.Geometry.Vertex
import Yage.Geometry.Elements

---------------------------------------------------------------------------------------------------
-- Primitives


cube :: (Fractional a) => V3 a -> Primitive (Vertex (P3 pn a))
cube dim = 
  let V3 x y z  = dim / 2.0
      right     = Face (V3   x  y   z ) (V3   x  (-y) z) (V3 x (-y) (-z)) (V3 x y (-z))
      left      = flipFace right & mapped._x *~ (-1)
      
      top       = Face (V3 (-x) y (-z)) (V3 (-x)   y  z) (V3 x   y    z ) (V3 x y (-z))
      bottom    = flipFace top & mapped._y *~ (-1)
      
      front     = Face (V3 (-x) y   z ) (V3 (-x) (-y) z) (V3 x (-y)   z)  (V3 x y   z )
      back      = flipFace front & mapped._z *~ (-1) 
  in Cube 
        ((position3 =:) <$> right) ((position3 =:) <$> left)
        ((position3 =:) <$> top)   ((position3 =:) <$> bottom)
        ((position3 =:) <$> front) ((position3 =:) <$> back)
    



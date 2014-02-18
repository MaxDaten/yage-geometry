{-# LANGUAGE RecordWildCards #-}
module Yage.Primitives.D3.Cube where

import Yage.Prelude hiding (Index)

import Linear (V3(..))
import Yage.Primitives.D3.Basic
import Yage.Vertex

---------------------------------------------------------------------------------------------------
-- Primitives


cube :: (Fractional a) => V3 a -> Primitive (Vertex (P3 pn a))
cube dim = 
  let V3 x y z  = dim / 2.0
      right     = (position3 =:) <$> Face (V3   x  y   z ) (V3   x  (-y) z) (V3 x (-y) (-z)) (V3 x y (-z))
      left      = flipFace right
      top       = (position3 =:) <$> Face (V3 (-x) y (-z)) (V3 (-x)   y  z) (V3 x   y    z ) (V3 x y (-z))
      bottom    = flipFace top
      front     = (position3 =:) <$> Face (V3 (-x) y   z ) (V3 (-x) (-y) z) (V3 x (-y)   z)  (V3 x y   z )
      back      = flipFace front
  in Cube right left top bottom front back
    



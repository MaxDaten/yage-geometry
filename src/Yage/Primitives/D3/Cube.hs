{-# LANGUAGE RecordWildCards #-}
module Yage.Primitives.D3.Cube where

import Yage.Prelude hiding (Index)

import Linear (V3(..))
import Yage.Primitives.D3.Basic

---------------------------------------------------------------------------------------------------
-- Primitives


cube :: (Fractional v) => V3 v -> Primitive (V3 v) -- Vertex3P3N3C4T2
cube dim = 
  let V3 x y z  = dim / 2.0
      right     = Face (V3   x  y   z ) (V3   x  (-y) z) (V3 x (-y) (-z)) (V3 x y (-z))
      left      = flipFace right
      top       = Face (V3 (-x) y (-z)) (V3 (-x)   y  z) (V3 x   y    z ) (V3 x y (-z))
      bottom    = flipFace top
      front     = Face (V3 (-x) y   z ) (V3 (-x) (-y) z) (V3 x (-y)   z)  (V3 x y   z )
      back      = flipFace front
  in Cube right left top bottom front back
    



{-# LANGUAGE RecordWildCards #-}
module Yage.Primitives.D3.Cube where

import Yage.Prelude hiding (Index)
import Yage.Lens

import Linear (V2(..), V3(..), _x, _y, _z)
import Yage.Primitives.D3.Basic
import Yage.Geometry.Vertex
import Yage.Geometry.Elements

---------------------------------------------------------------------------------------------------
-- Primitives


cube :: (Fractional a) => V3 a -> Primitive (Vertex (P3T2NT3 pn tx nn tg a))
cube dim = 
  let V3 x y z  = dim / 2.0
      right     = Face  (pos =: (V3 x   y    z ) <+> tex =: (V2 0 0) <+> norm =: (V3 1 0 0) <+> tang =: (V3 0 0 (-1)))
                        (pos =: (V3 x (-y)   z ) <+> tex =: (V2 1 0) <+> norm =: (V3 1 0 0) <+> tang =: (V3 0 0 (-1)))
                        (pos =: (V3 x (-y) (-z)) <+> tex =: (V2 1 1) <+> norm =: (V3 1 0 0) <+> tang =: (V3 0 0 (-1)))
                        (pos =: (V3 x   y  (-z)) <+> tex =: (V2 0 1) <+> norm =: (V3 1 0 0) <+> tang =: (V3 0 0 (-1)))

      left      = Face  (pos =: (V3 (-x)   y  (-z)) <+> tex =: (V2 0 0) <+> norm =: (V3 (-1) 0 0) <+> tang =: (V3 0 0 1))
                        (pos =: (V3 (-x) (-y) (-z)) <+> tex =: (V2 1 0) <+> norm =: (V3 (-1) 0 0) <+> tang =: (V3 0 0 1))
                        (pos =: (V3 (-x) (-y)   z ) <+> tex =: (V2 1 1) <+> norm =: (V3 (-1) 0 0) <+> tang =: (V3 0 0 1))
                        (pos =: (V3 (-x)   y    z ) <+> tex =: (V2 0 1) <+> norm =: (V3 (-1) 0 0) <+> tang =: (V3 0 0 1))
      
      top       = Face  (pos =: (V3 (-x) y (-z)) <+> tex =: (V2 0 0) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))
                        (pos =: (V3 (-x) y   z ) <+> tex =: (V2 1 0) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))
                        (pos =: (V3   x  y   z ) <+> tex =: (V2 1 1) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))
                        (pos =: (V3   x  y (-z)) <+> tex =: (V2 0 1) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))

      bottom    = Face  (pos =: (V3   x  (-y) (-z)) <+> tex =: (V2 0 0) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 1))
                        (pos =: (V3   x  (-y)   z ) <+> tex =: (V2 1 0) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 1))
                        (pos =: (V3 (-x) (-y)   z ) <+> tex =: (V2 1 1) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 1))
                        (pos =: (V3 (-x) (-y) (-z)) <+> tex =: (V2 0 1) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 1))
      
      front     = Face  (pos =: (V3 (-x)   y  z) <+> tex =: (V2 0 0) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))
                        (pos =: (V3 (-x) (-y) z) <+> tex =: (V2 1 0) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))
                        (pos =: (V3   x  (-y) z) <+> tex =: (V2 1 1) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))
                        (pos =: (V3   x    y  z) <+> tex =: (V2 0 1) <+> norm =: (V3 0 1 0) <+> tang =: (V3 1 0 0))

      back      = Face  (pos =: (V3   x    y  (-z)) <+> tex =: (V2 0 0) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 0))
                        (pos =: (V3   x  (-y) (-z)) <+> tex =: (V2 1 0) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 0))
                        (pos =: (V3 (-x) (-y) (-z)) <+> tex =: (V2 1 1) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 0))
                        (pos =: (V3 (-x)   y  (-z)) <+> tex =: (V2 0 1) <+> norm =: (V3 0 (-1) 0) <+> tang =: (V3 (-1) 0 0))
  in Cube right left top bottom front back
  where
    pos = position3
    tex = texture2
    norm = normal3
    tang = tangent3

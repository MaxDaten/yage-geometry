{-# LANGUAGE RecordWildCards #-}
module Yage.Geometry.D3.Cube where

import Yage.Prelude hiding (Index)

import Linear (V2(..), V3(..))
import Yage.Geometry.D3.Basic
import Yage.Geometry.Vertex
import Yage.Geometry.Elements

---------------------------------------------------------------------------------------------------
-- Primitives


cube :: (Fractional a) => V3 a -> Primitive (Vertex (P3T2NT3 pn tx nn tg a))
cube dim = 
  let V3 x y z  = dim / 2.0
                        -- position               | uv       | normal        | tangent
      right     = Face  ( vertex (V3 x   y    z )    (V2 0 0)  (V3 1 0 0)      (V3 0 0 (-1)) )
                        ( vertex (V3 x (-y)   z )    (V2 1 0)  (V3 1 0 0)      (V3 0 0 (-1)) )
                        ( vertex (V3 x (-y) (-z))    (V2 1 1)  (V3 1 0 0)      (V3 0 0 (-1)) )
                        ( vertex (V3 x   y  (-z))    (V2 0 1)  (V3 1 0 0)      (V3 0 0 (-1)) )

      left      = Face  ( vertex (V3 (-x)   y  (-z)) (V2 0 0)  (V3 (-1) 0 0)   (V3 0 0 1) )
                        ( vertex (V3 (-x) (-y) (-z)) (V2 1 0)  (V3 (-1) 0 0)   (V3 0 0 1) )
                        ( vertex (V3 (-x) (-y)   z ) (V2 1 1)  (V3 (-1) 0 0)   (V3 0 0 1) )
                        ( vertex (V3 (-x)   y    z ) (V2 0 1)  (V3 (-1) 0 0)   (V3 0 0 1) )
      
      top       = Face  ( vertex (V3 (-x) y (-z))    (V2 0 0)  (V3 0 1 0)      (V3 1 0 0) )
                        ( vertex (V3 (-x) y   z )    (V2 1 0)  (V3 0 1 0)      (V3 1 0 0) )
                        ( vertex (V3   x  y   z )    (V2 1 1)  (V3 0 1 0)      (V3 1 0 0) )
                        ( vertex (V3   x  y (-z))    (V2 0 1)  (V3 0 1 0)      (V3 1 0 0) )

      bottom    = Face  ( vertex (V3   x  (-y) (-z)) (V2 0 0)  (V3 0 (-1) 0)   (V3 (-1) 0 1) )
                        ( vertex (V3   x  (-y)   z ) (V2 1 0)  (V3 0 (-1) 0)   (V3 (-1) 0 1) )
                        ( vertex (V3 (-x) (-y)   z ) (V2 1 1)  (V3 0 (-1) 0)   (V3 (-1) 0 1) )
                        ( vertex (V3 (-x) (-y) (-z)) (V2 0 1)  (V3 0 (-1) 0)   (V3 (-1) 0 1) )
      
      front     = Face  ( vertex (V3 (-x)   y  z)    (V2 0 0)  (V3 0 0 1)      (V3 1 0 0) )
                        ( vertex (V3 (-x) (-y) z)    (V2 1 0)  (V3 0 0 1)      (V3 1 0 0) )
                        ( vertex (V3   x  (-y) z)    (V2 1 1)  (V3 0 0 1)      (V3 1 0 0) )
                        ( vertex (V3   x    y  z)    (V2 0 1)  (V3 0 0 1)      (V3 1 0 0) )

      back      = Face  ( vertex (V3   x    y  (-z)) (V2 0 0)  (V3 0 0 (-1))   (V3 (-1) 0 0) )
                        ( vertex (V3   x  (-y) (-z)) (V2 1 0)  (V3 0 0 (-1))   (V3 (-1) 0 0) )
                        ( vertex (V3 (-x) (-y) (-z)) (V2 1 1)  (V3 0 0 (-1))   (V3 (-1) 0 0) )
                        ( vertex (V3 (-x)   y  (-z)) (V2 0 1)  (V3 0 0 (-1))   (V3 (-1) 0 0) )
  in Cube right left top bottom front back
  where
    vertex :: V3 a -> V2 a -> V3 a -> V3 a -> Vertex (P3T2NT3 pn tx nn tg a)
    vertex p t n b = position3 =: p <+>
                     texture2  =: t <+>
                     normal3   =: n <+>
                     tangent3  =: b

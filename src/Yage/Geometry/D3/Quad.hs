{-# LANGUAGE TemplateHaskell #-}

module Yage.Geometry.D3.Quad where

import Yage.Prelude
import Yage.Lens

import Linear
import Yage.Geometry.Elements

---------------------------------------------------------------------------------------------------
data Quad v = Quad { _quadFace :: Face v }
    deriving ( Show, Functor, Foldable, Traversable, Generic )

makeLenses ''Quad

quad :: (Floating a) => V2 a -> Quad (V3 a)
quad dim = 
    let V2 x y = dim / 2.0
        tl     = V3 (-x)   y  0.0
        tr     = V3   x    y  0.0
        br     = V3   x  (-y) 0.0
        bl     = V3 (-x) (-y) 0.0
    in Quad $ Face tl bl br tr


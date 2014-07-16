{-# LANGUAGE TemplateHaskell #-}

module Yage.Geometry.D3.Pyramid where

import Yage.Prelude
import Yage.Lens

import Yage.Data.List (shift)
import Yage.Math

import Yage.Geometry.Elements

data Pyramid v = Pyramid 
    { _pyramidMantle :: [Triangle v]
    , _pyramidBase   :: [Triangle v] 
    } deriving ( Show, Functor, Foldable, Traversable, Generic )

makeLenses ''Pyramid
    

pyramid :: (Floating a, Enum a, Real a) => V3 a -> Pyramid (V3 a)
pyramid dim = 
    let (V3 x h z)  = (realToFrac <$> dim) / V3 2 1 2
        tip         = V3 0 h 0
        basev       = [ V3 (-x) 0 z, V3 x 0 z, V3 x 0 (-z), V3 (-x) 0 (-z) ]
        mantle      = [ Triangle tip a b  | (a, b)    <- zip (shift basev) basev ]
        base        = [ Triangle a b c    | (a, b, c) <- zip3 basev (shift basev) (shift $ shift basev) ] 
    in Pyramid mantle base

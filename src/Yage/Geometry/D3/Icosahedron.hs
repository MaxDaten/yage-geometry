{-# LANGUAGE TemplateHaskell #-}

module Yage.Geometry.D3.Icosahedron where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Data.List (shift, init, transpose)

import Yage.Geometry.Elements

data Icosahedron v = Icosahedron 
    { _icoTop        :: [Triangle v]
    , _icoMiddle     :: [Triangle v]
    , _icoBottom     :: [Triangle v]
    } deriving ( Show, Functor, Foldable, Traversable, Generic )

makeLenses ''Icosahedron
    

icosahedron :: (Floating a, Enum a) => Double -> Icosahedron (V3 a)
icosahedron radius = Icosahedron top middle bottom 
    where r             = realToFrac radius
          north         = V3 0   r  0
          south         = V3 0 (-r) 0
          topv          = [ V3 (r * cos a * sin theta) (  r  * cos theta) (r * sin a * sin theta)   | a <- init [ 0     , 2 * pi / 5 .. 2 * pi ] ]
          botv          = [ V3 (r * cos a * sin theta) ((-r) * cos theta) (r * sin a * sin theta)   | a <- init [ pi / 5, 3 * pi / 5 .. 2 * pi + pi / 5 ] ]
          V3 _ y _      = signorm $ V3 0 1 ((1 + sqrt 5)/2)
          theta         = acos y
          top           = [ Triangle north a b | (a, b) <- zip topv (shift topv) ]
          bottom        = [ Triangle south a b | (a, b) <- zip (shift botv) (botv) ]
          middleTop     = zipWith3 Triangle (shift topv) topv (shift botv)
          middleBottom  = zipWith3 Triangle botv (shift botv) (topv)
          middle        = concat $ transpose [middleTop, middleBottom]


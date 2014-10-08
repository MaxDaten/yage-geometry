{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Yage.Geometry.D3.Cone where

import           Yage.Lens
import           Yage.Prelude

import           Yage.Data.List         (init, shift)
import           Yage.Math

import           Yage.Geometry.Elements

data Cone v = Cone
    { _coneMantle :: [Triangle v]
    , _coneBase   :: [Triangle v]
    } deriving ( Show, Functor, Foldable, Traversable, Generic )

makeLenses ''Cone


-- | Cone with tip at (0, height, 0) and base center at origin
cone :: (Floating a, Enum a) => Double -> Double -> Int -> Cone (V3 a)
cone radius height divs =
    let h           = realToFrac height
        r           = realToFrac radius
        d           = realToFrac divs
        tip         = V3 0 h 0
        baseCenter  = V3 0 0 0
        basev       = [ V3 (r * cos a ) 0 (r * sin a) | a <- init [0, 2 * pi / d .. 2 * pi ] ]
        mantle      = [ Triangle tip a b        | (a, b) <- zip basev (shift basev) ]
        base        = [ Triangle baseCenter a b | (a, b) <- zip (shift basev) basev ]
    in Cone mantle base


instance HasSurfaces Cone where
    surfaces Cone{..} = [ Surface $ vertices _coneMantle, Surface $ vertices _coneBase ]


instance HasTriangles Cone where
    triangles Cone{..} = _coneMantle ++ _coneBase


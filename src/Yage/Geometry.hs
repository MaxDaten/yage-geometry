{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Yage.Geometry
    ( module Yage.Geometry
    , module Elements
    ) where

import Yage.Prelude

import Data.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Binary ()
import Yage.Geometry.Elements as Elements
import GHC.Generics (Generic)


data Geometry e = Geometry
    { geoElements :: V.Vector e
    } deriving ( Show, Eq, Functor, Foldable, Traversable, Generic )



instance (Binary e) => Binary (Geometry e)

--instance Applicative Geometry where
--    pure = Geometry . V.singleton
--    (Geometry f) <*> (Geometry v) = Geometry (f <*> v)


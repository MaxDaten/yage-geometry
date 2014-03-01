{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Yage.Geometry
    ( module Yage.Geometry
    , module Elements
    ) where

import Yage.Prelude
import Yage.Lens hiding (elements)

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Vector.Binary ()
import Data.Vinyl.Binary
import Data.Binary
import GHC.Generics (Generic)

import Yage.Geometry.Vertex
import Yage.Geometry.Elements as Elements

data Geometry e = Geometry
    { geoElements :: V.Vector e
    } deriving ( Show, Functor, Foldable, Traversable, Generic )

instance Applicative Geometry where
    pure = Geometry . V.singleton
    (Geometry f) <*> (Geometry v) = Geometry (f <*> v)


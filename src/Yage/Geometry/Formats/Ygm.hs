{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
module Yage.Geometry.Formats.Ygm where

import Yage.Prelude

import GHC.Generics (Generic)

import Data.Text
import Data.Proxy
import Data.Binary
import Data.Text.Binary


import Yage.Geometry
import Yage.Geometry.Vertex
import Yage.Geometry.Elements

-- yage geometry model
data YGM e = YGM
    { ygmName  :: Text
    , ygmModel :: Geometry (Triangle (Vertex e))
    } deriving ( Typeable, Generic )



ygmToFile :: (Binary a) => FilePath -> YGM (P3T2 pn tn a) -> IO ()
ygmToFile = encodeFile . fpToString

ygmFromFile :: (Binary a) => FilePath -> Proxy (P3T2 pn tn a) -> IO (YGM (P3T2 pn tn a))
ygmFromFile path _p = YGM (fpToText path) <$> (decodeFile . fpToString) path


instance Show (YGM e) where
    show YGM{ygmName} = format "YGM {name = {0}}" [show ygmName]

instance Binary (Vertex e) => Binary (YGM e)
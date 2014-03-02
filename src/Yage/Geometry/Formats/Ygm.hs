{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
module Yage.Geometry.Formats.Ygm where

import Yage.Prelude

import GHC.Generics (Generic)

import Data.Proxy
import Data.Binary
import Data.Text.Binary ()
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as B

import Yage.Geometry
import Yage.Geometry.Vertex

-- yage geometry model
data YGM e = YGM
    { ygmName  :: Text
    , ygmModel :: Geometry (Triangle (Vertex e))
    } deriving ( Typeable, Generic )



ygmToFile :: (v ~ (P3T2 pn tn a), Binary (Vertex v)) => FilePath -> YGM v -> IO ()
ygmToFile name = B.writeFile (fpToString name) . compress . encode

ygmFromFile :: (v ~ (P3T2 pn tn a), Binary (Vertex v)) => FilePath -> Proxy v -> IO (YGM v)
ygmFromFile path _p = decode . decompress <$> (B.readFile $ fpToString path)


instance Show (YGM e) where
    show YGM{ygmName} = format "YGM {name = {0}}" [show ygmName]

instance Binary (Vertex e) => Binary (YGM e)

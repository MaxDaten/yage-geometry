{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
module Yage.Geometry.Formats.Ygm
    ( module Yage.Geometry.Formats.Ygm
    ) where

import Yage.Prelude

import GHC.Generics (Generic)

import Data.Proxy
import Data.Binary
import Data.Text.Binary as Bin ()
import Data.Vinyl.Binary as Bin ()
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as B

import Yage.Geometry
import Yage.Geometry.Vertex

-- yage geometry model
data YGM e = YGM
    { ygmName  :: Text
    , ygmModel :: Geometry (Vertex e) (Triangle Int)
    } deriving ( Typeable, Generic )



ygmToFile :: (v ~ (P3T2NT3 pn txn nn tgn a), Binary (Vertex v)) 
          => FilePath -> YGM v -> IO ()
ygmToFile name = B.writeFile (fpToString name) . compress . encode

ygmFromFile :: (v ~ (P3T2NT3 pn txn nn tgn a), Binary (Vertex v)) 
            => FilePath -> Proxy v -> IO (YGM v)
ygmFromFile path _p = decode . decompress <$> (B.readFile $ fpToString path)


vertexFormat :: (v ~ (P3T2NT3 pn txn nn tgn a), Binary (Vertex v)) 
             => Pos a -> Tex a -> NT a -> (Vertex v)
vertexFormat pos tex (normal, tangent) =
    position3 =: pos <+>
    texture2  =: tex <+>
    normal3   =: normal <+>
    tangent3  =: tangent

instance Show (YGM e) where
    show YGM{ygmName} = format "YGM {name = {0}}" [show ygmName]

instance Binary (Vertex e) => Binary (YGM e)

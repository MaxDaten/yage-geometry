{-# LANGUAGE DeriveGeneric      #-}
module Yage.Geometry.Formats.Obj
    ( module Yage.Geometry.Formats.Obj
    , module Yage.Geometry.Formats.Obj.Parser
    ) where

import Yage.Prelude
import Yage.Lens hiding (elements)

import GHC.Generics (Generic)

import Data.Proxy
import qualified Data.Vector as V
import Yage.Geometry.Formats.Obj.Parser hiding (Face)
import Yage.Geometry.Vertex
import Yage.Geometry.Elements
import Yage.Geometry



geometryFromOBJ :: (Floating a, Enum a) => Proxy (P3T2 pn tn a) -> OBJ -> Geometry (Triangle (Vertex (P3T2 pn tn a)))
geometryFromOBJ _p obj 
    | not $ hasTextureCoords obj  = error "OBJ is missing neccessary texture coords"
    | otherwise =
        Geometry { geoElements = V.concatMap createFace (obj^.elements.faces) }
        where -- createFace :: OBJ.Face -> Face (Vertex (P3 pn a))
            createFace (a:b:c:d:[]) = V.fromList . triangles $ Face (mkVertex a) (mkVertex b) (mkVertex c) (mkVertex d)
            createFace (a:b:c:[])   =            V.singleton $ Triangle (mkVertex a) (mkVertex b) (mkVertex c)
            createFace _            = error "Yage.Geometry.geometryFromOBJ: invalid obj face"
              
            mkVertex ((VertexIndex  vi):(TextureIndex ti):_ixs) =
                position3 =: (realToFrac <$> verts V.! (vi-1)) <+> 
                texture2  =: (realToFrac <$> texs V.! (ti-1))
            mkVertex (_:ixs) = mkVertex ixs
            mkVertex []      = error "Yage.Geometry.geometryFromOBJ: missing vertex data"
              
            verts = obj^.vertexData.geometricVertices
            texs  = obj^.vertexData.textureVertices

geometryFromOBJFile :: (Floating a, Enum a) => Proxy (P3T2 pn tn a) -> FilePath -> IO (Geometry (Triangle (Vertex (P3T2 pn tn a))))
geometryFromOBJFile p file = (geometryFromOBJ p) <$> parseOBJFile file


hasTextureCoords :: OBJ -> Bool
hasTextureCoords obj = not . V.null $ obj^.vertexData.textureVertices

hasNormals :: OBJ -> Bool
hasNormals obj = not . V.null $ obj^.vertexData.vertexNormals
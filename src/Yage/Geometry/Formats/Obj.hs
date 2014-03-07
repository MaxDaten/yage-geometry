{-# LANGUAGE DeriveGeneric      #-}
module Yage.Geometry.Formats.Obj
    ( module Yage.Geometry.Formats.Obj
    , module Yage.Geometry.Formats.Obj.Parser
    ) where

import Yage.Prelude
import Yage.Lens hiding (elements)

import Data.Proxy
import qualified Data.Vector as V
import Yage.Geometry.Formats.Obj.Parser hiding (Face)
import Yage.Geometry.Vertex
import Yage.Geometry.Elements
import Yage.Geometry



type Geo pn tn a = TriGeo (Vertex (P3T2 pn tn a))

geometryFromOBJ :: (Floating a, Enum a, Show a) => Proxy (P3T2 pn tn a) -> OBJ -> Geo pn tn a
geometryFromOBJ _p obj 
    | not $ hasTextureCoords obj  = error "OBJ is missing neccessary texture coords"
    | otherwise = 
        Geometry { geoVertices = V.concatMap (V.fromList . map mkVertex) (obj^.elements.faces)
                 , geoElements = V.concatMap id $ V.generate (V.length $ obj^.elements.faces) (\i -> V.fromList . triangles $ Face (i*4) (i*4+1) (i*4+2) (i*4+3))
                 }
        where -- createFace :: OBJ.Face -> Face (Vertex (P3 pn a))
            --createElement (a:b:c:d:[]) = V.fromList . triangles $ traceShowS' "theFace:" $ Face (idx a) (idx b) (idx c) (idx d)
            --createElement (a:b:c:[])   =            V.singleton $ traceShowS' "theTri:" $ Triangle (idx a) (idx b) (idx c)
            --createElement _            = error "Yage.Geometry.geometryFromOBJ: invalid obj face"
            
            --idx (VertexIndex vi:_ixs) = vi - 1
            --idx (_:ixs) = idx ixs
            --idx [] = error "Yage.Geometry.geometryFromOBJ: missing VertexIndex in OBJ"

            mkVertex ((VertexIndex  vi):(TextureIndex ti):_ixs) =
                position3 =: (realToFrac <$> verts V.! (vi-1)) <+> 
                texture2  =: (realToFrac <$> texs V.! (ti-1))
            mkVertex (_:ixs) = traceShow "skipping" $ mkVertex ixs
            mkVertex []      = error "Yage.Geometry.geometryFromOBJ: missing vertex data"
              
            verts = obj^.vertexData.geometricVertices
            texs  = obj^.vertexData.textureVertices


geometryFromOBJFile :: (Floating a, Enum a, Show a) => Proxy (P3T2 pn tn a) -> FilePath -> IO (Geo pn tn a)
geometryFromOBJFile p file = (geometryFromOBJ p) <$> parseOBJFile file


hasTextureCoords :: OBJ -> Bool
hasTextureCoords obj = not . V.null $ obj^.vertexData.textureVertices

hasNormals :: OBJ -> Bool
hasNormals obj = not . V.null $ obj^.vertexData.vertexNormals
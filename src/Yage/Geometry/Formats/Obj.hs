{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TemplateHaskell    #-}
module Yage.Geometry.Formats.Obj
    ( module Yage.Geometry.Formats.Obj
    , module Yage.Geometry.Formats.Obj.Parser
    ) where

import Yage.Prelude hiding (any)
import Yage.Lens hiding (elements)
import Yage.Math
import Data.List (nub)
import Data.Proxy
import Data.Foldable (any)
import qualified Data.Vector as V
import Yage.Geometry.Formats.Obj.Parser hiding (Face)
import qualified Yage.Geometry.Formats.Obj.Parser as OBJ (Face)
import Yage.Geometry.Vertex
import Yage.Geometry.Elements
import Yage.Geometry




type Geo pn tn a = TriGeo (Vertex (P3T2 pn tn a))


type FaceIdx = Int
type VertIdx = Int
type TexIdx  = Int

data OBJFaceVertex = FaceVertex
    { fVertexIndex  :: VertIdx
    , fTextureIndex :: TexIdx
    }

geometryFromOBJ :: (Floating a, Enum a) => Proxy (P3T2 pn tn a) -> OBJ -> Geo pn tn a
geometryFromOBJ _p obj 
    | not $ hasTextureCoords obj  = error "OBJ is missing neccessary texture coords"
    | otherwise = 
        let (mergedVertices, reorganizedIndices) = V.ifoldl splitMergeVertexWithTex (V.empty, vIndices) verts
        in Geometry{geoVertices = mergedVertices, geoElements = reorganizedIndices}
    where 
    
    splitMergeVertexWithTex :: (Floating a, Enum a) 
                            => (V.Vector (Vertex (P3T2 pn tn a)), V.Vector (Triangle Int)) 
                            -> VertIdx -> V3 Float
                            -> (V.Vector (Vertex (P3T2 pn tn a)), V.Vector (Triangle Int)) 
    splitMergeVertexWithTex (mergedVerts, updatedFaces) vidx _vertex =
        let offset      = V.length mergedVerts - vidx
            shared      = V.toList $ sharedFaces vidx
            -- get unique texture indices of the vertex-index for the shared faces
            texIdxs     = nub $ map (extractTexIdx vidx) shared
            -- create Vertex with position and texture attribute
            texedVerts  = map (\(faceIndex, texIndex) -> (faceIndex, emitVertex vidx texIndex)) texIdxs
            -- create old id to new id mappings for the shared faces to match the indices of newly created vertices 
            remappings  = map (\(i,(faceId,_)) -> (faceId, (vidx, (vidx + i + offset)))) $ zip [0..] texedVerts
        in (mergedVerts ++ (V.fromList $ map snd texedVerts), V.accum replaceVertexIndex updatedFaces remappings)

    emitVertex vertIdx texIdx =
        position3 =: (realToFrac <$> (verts V.! (vertIdx-1))) <+> 
        texture2  =: (realToFrac <$> (texs V.! (texIdx-1)))

    replaceVertexIndex :: Triangle VertIdx -> (VertIdx, VertIdx) -> Triangle VertIdx
    replaceVertexIndex tri (was, isNow) = (\i -> if i == was then isNow else i) <$> tri

    isFaceOf :: Int -> Triangle OBJFaceVertex -> Bool -- | Face = [References]; References = [Index]
    isFaceOf vertexIndex = any (\face -> fVertexIndex face == vertexIndex)

    extractTexIdx :: VertIdx -> (FaceIdx, Triangle OBJFaceVertex) -> (FaceIdx, TexIdx)
    extractTexIdx vertexIndex (i, triFace) = (i, fTextureIndex $ getVertex vertexIndex triFace)

    createFaceVert :: OBJ.Face -> V.Vector (Triangle OBJFaceVertex)
    createFaceVert (a:b:c:d:[]) = V.fromList . triangles $ Face (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c) (mkFaceVertex d)
    createFaceVert (a:b:c:[])   = V.singleton $ Triangle (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c)
    createFaceVert _ = error "Yage.Geometry.Formats.Obj: invalid Face in OBJ"

    mkFaceVertex :: References -> OBJFaceVertex
    mkFaceVertex ref = FaceVertex (refVertexIndex ref) (refTextureIndex ref)
    
    refVertexIndex :: References -> Int
    refVertexIndex ((VertexIndex vi):_) = vi
    refVertexIndex _ = error "Yage.Geometry.Formats.Obj.refVertexIndex: invalid index order"
    
    refTextureIndex :: References -> Int
    refTextureIndex (_vi:(TextureIndex ti):_) = ti
    refTextureIndex _ = error "Yage.Geometry.Formats.Obj.refTextureIndex: invalid index order"
    
    verts   = obj^.vertexData.geometricVertices
    texs    = obj^.vertexData.textureVertices
    elems   = obj^.elements.faces
    
    triFaces :: V.Vector (Int, Triangle OBJFaceVertex)
    triFaces = V.indexed $ V.concatMap createFaceVert elems
    
    vIndices :: V.Vector (Triangle Int)
    vIndices = V.map (\(_i, tri) -> fmap fVertexIndex tri) triFaces

    sharedFaces :: VertIdx -> V.Vector (FaceIdx, Triangle OBJFaceVertex)
    sharedFaces vidx = V.filter (\(_,face) -> isFaceOf vidx face) triFaces

    getVertex :: VertIdx -> Triangle OBJFaceVertex -> OBJFaceVertex
    getVertex vidx (Triangle a b c)
        | fVertexIndex a == vidx = a
        | fVertexIndex b == vidx = b
        | fVertexIndex c == vidx = c
        | otherwise = error $ format "Yage.Geometry.Formats.Obj.getVertex: Triangle without vertex index: {0}" [show vidx]



geometryFromOBJFile :: (Floating a, Enum a, Show a) => Proxy (P3T2 pn tn a) -> FilePath -> IO (Geo pn tn a)
geometryFromOBJFile p file = (geometryFromOBJ p) <$> parseOBJFile file


hasTextureCoords :: OBJ -> Bool
hasTextureCoords obj = not . V.null $ obj^.vertexData.textureVertices

hasNormals :: OBJ -> Bool
hasNormals obj = not . V.null $ obj^.vertexData.vertexNormals
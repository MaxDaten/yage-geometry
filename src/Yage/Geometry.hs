module Yage.Geometry
    ( module Yage.Geometry
    , module Elements
    , module OBJ
    ) where

import Yage.Prelude hiding (elements)

import Data.Foldable (Foldable)

import qualified Data.Vector as V
import Data.Vector ((!))

import Yage.Geometry.Vertex
import Yage.Geometry.Elements as Elements
import Yage.Geometry.Formats.ObjParser hiding (Face)
import Yage.Geometry.Formats.ObjParser as OBJ (OBJ, name, parseOBJFile)



data Geometry e = Geometry
    { geoElements :: V.Vector e
    } deriving ( Show, Functor, Foldable, Traversable )


geometryFromOBJ :: (Floating a, Enum a) => OBJ -> Position3 pn a -> Geometry (Face (Vertex (P3 pn a)))
geometryFromOBJ obj posfield =
    Geometry { geoElements = V.map createFace (obj^.elements.faces) }
    where -- createFace :: OBJ.Face -> Face (Vertex (P3 pn a))
        createFace (a:b:c:d:[]) = Face (mkVertex a) (mkVertex b) (mkVertex c) (mkVertex d)
        createFace _            = error "Yage.Geometry.geometryFromOBJ: invalid obj face"
          
        mkVertex ((VertexIndex i):ixs) = posfield =: (realToFrac <$> verts ! (i-1))
        mkVertex (_:ixs) = mkVertex ixs
        mkVertex []      = error "Yage.Geometry.geometryFromOBJ: missing vertex data"
          
        verts = obj^.vertexData.geometricVertices

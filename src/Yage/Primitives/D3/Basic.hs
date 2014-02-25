{-# OPTIONS_GHC -fno-warn-name-shadowing    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Yage.Primitives.D3.Basic where

import Yage.Prelude

import Yage.Geometry.Vertex
import Yage.Geometry.Elements
import Data.List
import Data.Foldable (Foldable)

import Yage.Math


---------------------------------------------------------------------------------------------------
-- Primitives

data Primitive v = 
      Cone         { _coneMantle    :: [Triangle v]
                   , _coneBase      :: [Triangle v]
                   }
    
    | Cube         { _cubeRight     :: Face v        -- GL_TEXTURE_CUBE_MAP_POSITIVE_X​
                   , _cubeLeft      :: Face v        -- GL_TEXTURE_CUBE_MAP_NEGATIVE_X​
                   , _cubeTop       :: Face v        -- GL_TEXTURE_CUBE_MAP_POSITIVE_Y​
                   , _cubeBottom    :: Face v        -- GL_TEXTURE_CUBE_MAP_NEGATIVE_Y​
                   , _cubeFront     :: Face v        -- GL_TEXTURE_CUBE_MAP_POSITIVE_Z​
                   , _cubeBack      :: Face v        -- GL_TEXTURE_CUBE_MAP_NEGATIVE_Z​
                   } 
    
    | Grid         { _gridSections  :: [Face v] }
    
    | Icosahedron  { _icoTop        :: [Triangle v]
                   , _icoMiddle     :: [Triangle v]
                   , _icoBottom     :: [Triangle v]
                   }
    
    | Pyramid      { _pyramidMantle :: [Triangle v]
                   , _pyramidBase   :: [Triangle v] 
                   }
    
    | Quad         { _quadFace      :: Face v }
    
    | GeoSphere    { _geoSphereTris :: [Triangle v] }
    
    deriving ( Show, Functor, Foldable, Traversable )

makeLenses ''Primitive


calculateNormals :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v) 
                 => Position3 pn a -> Normal3 nn a -> NormalSmoothness -> Primitive (Vertex v) -> Primitive (Vertex vn)
calculateNormals pos norm smooth primitive = 
  let triangleNorm = addTriangleNormal pos norm
      faceNorm     = addFaceNormal pos norm
  in calc triangleNorm faceNorm primitive
  where
    calc triangleNorm _        Cone{..}        = Cone (fmap (triangleNorm smooth) _coneMantle) (fmap (triangleNorm FacetteNormals) _coneBase)
    calc _            faceNorm Cube{..}        = Cube 
                                                    ( faceNorm _cubeRight ) (faceNorm _cubeLeft   )
                                                    ( faceNorm _cubeTop   ) (faceNorm _cubeBottom )
                                                    ( faceNorm _cubeFront ) (faceNorm _cubeBack   )
    calc triangleNorm _        Icosahedron{..} = Icosahedron 
                                                    ( fmap (triangleNorm smooth) _icoTop )
                                                    ( fmap (triangleNorm smooth) _icoMiddle )
                                                    ( fmap (triangleNorm smooth) _icoBottom )
                                              
    calc _            faceNorm Grid{..}        = Grid $ faceNorm <$> _gridSections
    calc triangleNorm _        Pyramid{..}     = Pyramid 
                                                    ( fmap (triangleNorm smooth) _pyramidMantle )
                                                    ( fmap (triangleNorm smooth) _pyramidBase )
    calc _            faceNorm Quad{..}        = Quad $ faceNorm _quadFace
    calc triangleNorm _        GeoSphere{..}   = GeoSphere $ fmap (triangleNorm smooth) _geoSphereTris
calculateNormals _ _ _ _ = error "calculateNormals: unsupported primitive"



instance HasTriangles Primitive where
  triangles Cone{..}        = _coneMantle ++ _coneBase
  triangles Cube{..}        = triangles _cubeRight ++ triangles _cubeLeft ++
                              triangles _cubeTop   ++ triangles _cubeBottom ++
                              triangles _cubeFront ++ triangles _cubeBack
  triangles Icosahedron{..} = _icoTop ++ _icoMiddle ++ _icoBottom
  triangles Pyramid{..}     = _pyramidMantle ++ _pyramidBase
  triangles Quad{..}        = triangles _quadFace
  triangles GeoSphere{..}   = _geoSphereTris 
  triangles Grid{..}        = concat $ fmap triangles _gridSections
  triangles _ = error "invalid triangles for Primitive"


instance HasLines Primitive where
  toLines p = concatMap toLines $ triangles p



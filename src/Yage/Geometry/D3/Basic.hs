{-# OPTIONS_GHC -fno-warn-name-shadowing    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE LambdaCase   #-}
module Yage.Geometry.D3.Basic where

import Yage.Prelude
import Yage.Lens

import Yage.Geometry.Elements


---------------------------------------------------------------------------------------------------
-- Primitives


data Primitive v = 
      Cone         { _coneMantle    :: [Triangle v]
                   , _coneBase      :: [Triangle v]
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
    
    deriving ( Show, Functor, Foldable, Traversable , Generic)

makeLenses ''Primitive


instance HasTriangles Primitive where
  triangles Cone{..}        = _coneMantle ++ _coneBase
  triangles Icosahedron{..} = _icoTop ++ _icoMiddle ++ _icoBottom
  triangles Pyramid{..}     = _pyramidMantle ++ _pyramidBase
  triangles Quad{..}        = triangles _quadFace
  triangles GeoSphere{..}   = _geoSphereTris 
  triangles Grid{..}        = concat $ fmap triangles _gridSections
  -- triangles _ = error "invalid triangles for Primitive"


instance HasLines Primitive where
  toLines p = concatMap toLines $ triangles p

primitveSurfaces :: Primitive v -> [Surface (Triangle v)]
primitveSurfaces = \case
  Cone{..}          -> Surface <$> [_coneMantle, _coneBase]
  Grid{..}          -> singleton $ Surface $ concatMap triangles _gridSections
  Icosahedron{..}   -> singleton $ Surface $ concat [_icoTop, _icoMiddle, _icoBottom] 
  Pyramid{..}       -> Surface <$> [_pyramidMantle, _pyramidBase]
  Quad{..}          -> singleton $ Surface $ triangles _quadFace
  GeoSphere{..}     -> singleton $ Surface $ _geoSphereTris

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


instance HasTriangles Primitive where
  triangles Cone{..}        = _coneMantle ++ _coneBase
  triangles Icosahedron{..} = _icoTop ++ _icoMiddle ++ _icoBottom
  triangles Pyramid{..}     = _pyramidMantle ++ _pyramidBase
  triangles Quad{..}        = triangles _quadFace
  -- triangles _ = error "invalid triangles for Primitive"


instance HasLines Primitive where
  toLines p = concatMap toLines $ triangles p

primitveSurfaces :: Primitive v -> [Surface (Triangle v)]
primitveSurfaces = \case
  Cone{..}          -> Surface <$> [_coneMantle, _coneBase]
  Icosahedron{..}   -> singleton $ Surface $ concat [_icoTop, _icoMiddle, _icoBottom] 
  Pyramid{..}       -> Surface <$> [_pyramidMantle, _pyramidBase]
  Quad{..}          -> singleton $ Surface $ triangles _quadFace
  GeoSphere{..}     -> singleton $ Surface $ _geoSphereTris



{--
merge :: (Fractional a) => f (t (V3 a)) -> f (t (V2 a)) -> f (t (Vertex (P3T2 pn tn a)))
merge pos tex =
  let cube = cubePos dim
  in merge <$$> cube <**> cubeUV
  where
  merge :: (Fractional a, v ~ Vertex (P3T2 pn tn a)) => V3 a -> V2 a -> v
  merge p t = position3 =: p <+> 
              texture2  =: t
  
  (<**>) = liftA2 (<*>)
  f <$$> x = liftA2 (<$>) (pure f) x
--}

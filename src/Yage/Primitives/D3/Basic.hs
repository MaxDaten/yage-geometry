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

import Yage.Vertex
import Data.List
import Data.Foldable (Foldable)

import Yage.Math


---------------------------------------------------------------------------------------------------
-- Basic Types

data Triangle v = Triangle v v v
  deriving ( Functor, Foldable, Traversable )

data Face v = Face v v v v
  deriving ( Functor, Foldable, Traversable )


--data Surface v = Surface [Face v]
--  deriving ( Functor, Foldable, Traversable )

data NormalSmoothness = FacetteNormals | SphericalNormals


class HasTriangles p where
  triangles :: Functor p => p v -> [Triangle v]

instance HasTriangles Face where
  triangles (Face a b c d) = [Triangle a b c, Triangle c d a]

instance HasTriangles Triangle where
  triangles tri = [tri]


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



--calcFaceNormal :: (vn ~ (v ++ '[Normal3 nn a]), Epsilon a, Floating a, Implicit (Elem (Position3 pn a) v), Implicit (Elem (Position3 pn a) vn), Implicit (Elem (Normal3 nn a) vn)) => Face (Vertex v) -> Face (Vertex vn)
addFaceNormal :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v)
              => Position3 pn a -> Normal3 nn a -> Face (Vertex v) -> Face (Vertex vn)
addFaceNormal pos norm face@(Face a b c _) =
  let (n, _, _) = plainNormalForm (rGet pos c) (rGet pos b) (rGet pos a)
  in fmap (<+> norm =: n) face


addTriangleNormal :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v)
                  => Position3 pn a -> Normal3 nn a -> NormalSmoothness -> Triangle (Vertex v) -> Triangle (Vertex vn)
addTriangleNormal pos norm FacetteNormals t@(Triangle a b c) = 
  let (n, _, _) = plainNormalForm (rGet pos c) (rGet pos b) (rGet pos a)
  in fmap (<+> norm =: n) t
addTriangleNormal pos norm SphericalNormals t@(Triangle a b c) =
   Triangle (a <+> norm =: normalize (rGet pos a)) 
            (b <+> norm =: normalize (rGet pos b)) 
            (c <+> norm =: normalize (rGet pos c)) 

flipTriangle :: Triangle v -> Triangle v
flipTriangle (Triangle a b c) = Triangle c b a

flipFace :: Face v -> Face v
flipFace (Face a b c d) = Face d c b a

--flipSurface :: Surface v -> Surface v
--flipSurface (Surface faces) = Surface $ fmap flipFace faces


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
    
    deriving ( Functor, Foldable, Traversable )

makeLenses ''Primitive

cut :: (Num a, Epsilon a, Floating a)
    => a -> Position3 pn a -> Triangle (Vertex (P3 pn a)) -> [Triangle (Vertex (P3 pn a))]
cut r pos (Triangle a b c) = [Triangle a ab ac, Triangle b bc ab, Triangle c ac bc, Triangle ab bc ac]
    where ab = pos =: (hr *^ normalize (rGet pos a + rGet pos b))
          bc = pos =: (hr *^ normalize (rGet pos b + rGet pos c))
          ac = pos =: (hr *^ normalize (rGet pos a + rGet pos c))
          hr = r

 
triangulate :: (Epsilon a, Floating a) 
            => Int -> Position3 pn a -> [Triangle (Vertex (P3 pn a))] -> [Triangle (Vertex (P3 pn a))]
triangulate iter pos src = iterate subdivide src !! iter
    where subdivide      = concatMap cutR
          cutR           = cut (norm . fst3 . head $ src) pos
          fst3 (Triangle a _ _) = rGet pos a


calculateNormals :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v) 
                 => Position3 pn a -> Normal3 nn a -> NormalSmoothness -> Primitive (Vertex v) -> Primitive (Vertex vn)
calculateNormals pos norm smooth primitive = 
  let triangleNorm = addTriangleNormal pos norm smooth
      faceNorm     = addFaceNormal pos norm
  in calc triangleNorm faceNorm primitive
  where
    calc triangleNorm _        Cone{..}        = Cone (fmap triangleNorm _coneMantle) (fmap triangleNorm _coneBase)
    calc _            faceNorm Cube{..}        = Cube 
                                                    ( faceNorm _cubeRight ) (faceNorm _cubeLeft   )
                                                    ( faceNorm _cubeTop   ) (faceNorm _cubeBottom )
                                                    ( faceNorm _cubeFront ) (faceNorm _cubeBack   )
    calc triangleNorm _        Icosahedron{..} = Icosahedron 
                                                    ( fmap triangleNorm _icoTop )
                                                    ( fmap triangleNorm _icoMiddle )
                                                    ( fmap triangleNorm _icoBottom )
                                              
    calc _            faceNorm Grid{..}        = Grid $ faceNorm <$> _gridSections
    calc triangleNorm _        Pyramid{..}     = Pyramid 
                                                    ( fmap triangleNorm _pyramidMantle )
                                                    ( fmap triangleNorm _pyramidBase )
    calc _            faceNorm Quad{..}        = Quad $ faceNorm _quadFace
    calc triangleNorm _        GeoSphere{..}   = GeoSphere $ fmap triangleNorm _geoSphereTris
calculateNormals _ _ _ _ = error "calculateNormals: unsupported primitive"


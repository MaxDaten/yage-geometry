{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Yage.Primitives.D3.Basic where

import Yage.Prelude

import Data.List
import Data.Foldable (Foldable)

import Yage.Math


---------------------------------------------------------------------------------------------------
-- Basic Types

data Triangle v = Triangle v v v
  deriving ( Functor, Foldable, Traversable )

data Face v = Face v v v v
  deriving ( Functor, Foldable, Traversable )


type Normal   = V3
type Vertex   = V3
type TexCoord = V2

--data Surface v = Surface [Face v]
--  deriving ( Functor, Foldable, Traversable )



class HasTriangles p where
  triangles :: Functor p => p v -> [Triangle v]

instance HasTriangles Face where
  triangles (Face a b c d) = [Triangle a b c, Triangle c d a]

instance HasTriangles Triangle where
  triangles tri = [tri]


faceNormal :: (Epsilon a, Floating a) => Face (V3 a) -> V3 a
faceNormal (Face a b c _) = let (n, _, _) = plainNormalForm c b a in n



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

-- makeLenses ''Primitive

data NormalSmoothness = FacetteNormals | SphericalNormals


cut :: (Functor f, Num (f a), Num a, Epsilon a, Metric f, Floating a) 
    => a -> Triangle (f a) -> [Triangle (f a)]
cut r (Triangle a b c) = [Triangle a ab ac, Triangle b bc ab, Triangle c ac bc, Triangle ab bc ac]
    where ab = hr *^ normalize (a + b)
          bc = hr *^ normalize (b + c)
          ac = hr *^ normalize (a + c)
          hr = r

 
triangulate :: (Functor f, Num (f a), Num a, Epsilon a, Metric f, Floating a) 
            => Int -> [Triangle (f a)] -> [Triangle (f a)]
triangulate iter src    = iterate subdivide src !! iter
    where subdivide     = concatMap cutR
          cutR          = cut (norm . fst3 . head $ src)
          fst3 (Triangle a _ _) = a


calculateNormals :: (Floating v, Epsilon v) => Primitive (Vertex v) -> NormalSmoothness -> Primitive (Vertex v, Normal v)
calculateNormals Cone{..}        smooth  = Cone (fmap (normalCalculator smooth) _coneMantle) (fmap (normalCalculator FacetteNormals) _coneBase)
calculateNormals Cube{..}        _       = Cube (fmap (, faceNormal _cubeRight) _cubeRight  )  ( fmap (, faceNormal _cubeLeft   ) _cubeLeft   ) 
                                                (fmap (, faceNormal _cubeTop  ) _cubeTop    )  ( fmap (, faceNormal _cubeBottom ) _cubeBottom ) 
                                                (fmap (, faceNormal _cubeFront) _cubeFront  )  ( fmap (, faceNormal _cubeBack   ) _cubeBack   )
calculateNormals Grid{..}        _       = let n = faceNormal (head _gridSections) 
                                           in Grid $ map (fmap (,n)) _gridSections
calculateNormals Icosahedron{..} smooth  = Icosahedron ( fmap (normalCalculator smooth) _icoTop )
                                                       ( fmap (normalCalculator smooth) _icoMiddle )
                                                       ( fmap (normalCalculator smooth) _icoBottom )
calculateNormals Pyramid{..}     smooth  = Pyramid     ( fmap (normalCalculator smooth) _pyramidMantle )
                                                       ( fmap (normalCalculator smooth) _pyramidBase )
calculateNormals Quad{..}        _       = Quad      $ fmap (,faceNormal _quadFace) _quadFace
calculateNormals GeoSphere{..}   smooth  = GeoSphere $ fmap (normalCalculator smooth) _geoSphereTris
calculateNormals _ _ = error "calculateNormals: unsupported primitive"


normalCalculator :: (Epsilon v, Floating v) => NormalSmoothness -> (Triangle (Vertex v) -> (Triangle (Vertex v, Normal v)))
normalCalculator SphericalNormals = \(Triangle a b c) -> Triangle (a, normalize a) (b, normalize b) (c, normalize c) 
normalCalculator FacetteNormals   = \(Triangle a b c) -> let (n, _, _) = plainNormalForm c b a in Triangle (a, n) (b, n) (c, n)


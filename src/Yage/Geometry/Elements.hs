{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module Yage.Geometry.Elements where

import Yage.Prelude             hiding (head)
import Yage.Lens

import Yage.Geometry.Vertex
import Data.List                ( iterate, (!!), head )
import Data.Binary
import GHC.Generics (Generic)
import Yage.Math
import Linear.Binary



---------------------------------------------------------------------------------------------------
-- Basic Types

data Triangle v = Triangle v v v
  deriving ( Show, Functor, Foldable, Traversable, Generic )

data Face v = Face v v v v
  deriving ( Show, Functor, Foldable, Traversable, Generic )

data Line v = Line v v
  deriving ( Show, Functor, Foldable, Traversable, Generic )

data Point v = Point v
  deriving ( Show, Functor, Foldable, Traversable, Generic )


--data Surface v = Surface [Face v]
--  deriving ( Functor, Foldable, Traversable )

data NormalSmoothness = FacetteNormals | SphericalNormals


class HasTriangles p where
  triangles :: Functor p => p v -> [Triangle v]

instance HasTriangles Face where
  triangles (Face a b c d) = [Triangle a b c, Triangle c d a]

instance HasTriangles Triangle where
  triangles tri = [tri]



class HasLines p where
  toLines :: Functor p => p v -> [Line v]

instance HasLines Triangle where
  toLines (Triangle a b c) = [Line a b, Line b c, Line c a]

instance HasLines Face where
  toLines (Face a b c d) = [Line a b, Line b c, Line c d, Line d a]

vertices :: forall a (t :: * -> *) (t1 :: * -> *).
            (Traversable t1, Traversable t) =>
            t (t1 a) -> [a]
vertices p = p^..traverse.traverse


--calcFaceNormal :: (vn ~ (v ++ '[Normal3 nn a]), Epsilon a, Floating a, Implicit (Elem (Position3 pn a) v), Implicit (Elem (Position3 pn a) vn), Implicit (Elem (Normal3 nn a) vn)) => Face (Vertex v) -> Face (Vertex vn)
addFaceNormal :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v)
              => Position3 pn a -> Normal3 nn a -> Face (Vertex v) -> Face (Vertex vn)
addFaceNormal posF normF face@(Face a b c _) =
  let (n, _, _) = plainNormalForm (rGet posF c) (rGet posF b) (rGet posF a)
  in fmap (<+> normF =: n) face




addTriangleNormal :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v)
                  => Position3 pn a -> Normal3 nn a -> NormalSmoothness -> Triangle (Vertex v) -> Triangle (Vertex vn)
addTriangleNormal posF normF FacetteNormals t@(Triangle a b c) = 
  let (n, _, _) = plainNormalForm (rGet posF c) (rGet posF b) (rGet posF a)
  in fmap (<+> normF =: n) t
addTriangleNormal posF normF SphericalNormals (Triangle a b c) =
   Triangle (a <+> normF =: normalize (rGet posF a)) 
            (b <+> normF =: normalize (rGet posF b)) 
            (c <+> normF =: normalize (rGet posF c)) 



flipTriangle :: Triangle v -> Triangle v
flipTriangle (Triangle a b c) = Triangle c b a



flipFace :: Face v -> Face v
flipFace (Face a b c d) = Face d c b a



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



instance (Binary a, Applicative t, Foldable t, Traversable t) => Binary (t a) where
    put = putLinear
    get = getLinear


instance Applicative Point where
    pure v = Point v
    (Point f) <*> (Point a) = Point (f a)  

instance Applicative Line where
    pure v = Line v v
    (Line fa fb) <*> (Line a b) = Line (fa a) (fb b)

instance Applicative Triangle where
    pure v = Triangle v v v
    (Triangle fa fb fc) <*> (Triangle a b c) = Triangle (fa a) (fb b) (fc c) 

instance Applicative Face where
    pure v = Face v v v v
    (Face fa fb fc fd) <*> (Face a b c d) = (Face (fa a) (fb b) (fc c) (fd d))  


--instance (Binary e) => Binary (Triangle e)
--instance (Binary e) => Binary (Face e)
--instance (Binary e) => Binary (Line e)
--instance (Binary e) => Binary (Point e)
--flipSurface :: Surface v -> Surface v
--flipSurface (Surface faces) = Surface $ fmap flipFace faces


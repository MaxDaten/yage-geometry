{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleContexts   #-}

module Yage.Geometry.Elements where

import Yage.Prelude

import Yage.Geometry.Vertex
import Data.List
import Data.Foldable (Foldable)
import Yage.Math


---------------------------------------------------------------------------------------------------
-- Basic Types

data Triangle v = Triangle v v v
  deriving ( Show, Functor, Foldable, Traversable )

data Face v = Face v v v v
  deriving ( Show, Functor, Foldable, Traversable )

data Line v = Line v v
  deriving ( Show, Functor, Foldable, Traversable )

data Point v = Point v
  deriving ( Show, Functor, Foldable, Traversable )


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



--flipSurface :: Surface v -> Surface v
--flipSurface (Surface faces) = Surface $ fmap flipFace faces


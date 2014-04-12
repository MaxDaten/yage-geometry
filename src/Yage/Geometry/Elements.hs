{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Yage.Geometry.Elements where

import Yage.Prelude             hiding (head)
import Yage.Lens

import Foreign.Storable                (Storable(..))
import Foreign.Ptr                     (castPtr)

import Yage.Geometry.Vertex
import Data.List                       ( iterate, (!!), head )
import Data.Binary
import Yage.Math
-- import Linear.Binary



---------------------------------------------------------------------------------------------------
-- Basic Types

data Triangle v = Triangle v v v
  deriving ( Show, Eq, Functor, Foldable, Traversable, Generic )

data Face v = Face v v v v
  deriving ( Show, Eq, Functor, Foldable, Traversable, Generic )

data Line v = Line v v
  deriving ( Show, Eq, Functor, Foldable, Traversable, Generic )

data Point v = Point v
  deriving ( Show, Eq, Functor, Foldable, Traversable, Generic )


--data Surface v = Surface [Face v]
--  deriving ( Functor, Foldable, Traversable )

data NormalSmoothness = FacetteNormals | SphericalNormals


class HasTriangles p where
  triangles :: Functor p => p v -> [Triangle v]

instance HasTriangles Face where
  triangles (Face a b c d) = [Triangle a b c, Triangle a c d]

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
calcFaceNormal :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v)
              => Position3 pn a -> Normal3 nn a -> Face (Vertex v) -> Face (Vertex vn)
calcFaceNormal posF normF face@(Face a b c _) =
  let (n, _, _) = plainNormalForm (rGet posF c) (rGet posF b) (rGet posF a)
  in fmap (<+> normF =: n) face




calcTriangleNormal :: (Epsilon a, Floating a, vn ~ (v ++ '[Normal3 nn a]), IElem (Position3 pn a) v)
                  => Position3 pn a -> Normal3 nn a -> NormalSmoothness -> Triangle (Vertex v) -> Triangle (Vertex vn)
calcTriangleNormal posF normF FacetteNormals t@(Triangle a b c) = 
  let (n, _, _) = plainNormalForm (rGet posF c) (rGet posF b) (rGet posF a)
  in fmap (<+> normF =: n) t
calcTriangleNormal posF normF SphericalNormals (Triangle a b c) =
   Triangle (a <+> normF =: normalize (rGet posF a)) 
            (b <+> normF =: normalize (rGet posF b)) 
            (c <+> normF =: normalize (rGet posF c)) 


triangleNormal :: (Epsilon a, Floating a)
               => Triangle (V3 a) -> V3 a
triangleNormal = normalize . triangleUnnormal 


-- | area weighted triangle normal (the length is proportional to the area of the triangle)
triangleUnnormal :: (Epsilon a, Floating a)
               => Triangle (V3 a) -> V3 a
triangleUnnormal (Triangle a b c) = (b - a) `cross` (c - a)


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


instance (Binary e) => Binary (Triangle e)
instance (Binary e) => Binary (Face e)
instance (Binary e) => Binary (Line e)
instance (Binary e) => Binary (Point e)
--flipSurface :: Surface v -> Surface v
--flipSurface (Surface faces) = Surface $ fmap flipFace faces

instance Storable a => Storable (Triangle a) where
  sizeOf _ = 3 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (Triangle a b c) = do poke ptr' a
                                 pokeElemOff ptr' 1 b
                                 pokeElemOff ptr' 2 c
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = Triangle <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable a => Storable (Line a) where
  sizeOf _ = 2 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (Line a b) = do poke ptr' a
                           pokeElemOff ptr' 1 b
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = Line <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable a => Storable (Point a) where
  sizeOf _ = sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (Point a) = poke ptr' a
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = Point <$> peek ptr'
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable a => Storable (Face a) where
  sizeOf _ = 4 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (Face a b c d) = do poke ptr' a
                               pokeElemOff ptr' 1 b
                               pokeElemOff ptr' 2 c
                               pokeElemOff ptr' 3 d
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = Face <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2 <*> peekElemOff ptr' 3
    where ptr' = castPtr ptr
  {-# INLINE peek #-}


{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds  #-}

module Yage.Geometry
    ( module Yage.Geometry
    , module Elements
    ) where

import Yage.Prelude
import Yage.Math
import Yage.Lens

import Data.Vinyl
import Data.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Binary ()

import GHC.Generics (Generic)

import Yage.Geometry.Elements as Elements
import Yage.Geometry.Vertex


data Geometry v e = Geometry
    { geoVertices :: V.Vector v
    , geoElements :: V.Vector e     -- | like Triangle i, where i is the index to a vertex
    } deriving ( Show, Eq, Functor, Foldable, Traversable, Generic )

type TriGeo v = Geometry v (Triangle Int)

makeSimpleTriGeo :: V.Vector v -> TriGeo v
makeSimpleTriGeo verts = Geometry verts simpleIxs
    where
        triCnt = V.length verts `div` 3 
        simpleIxs = V.zipWith3 Triangle (V.generate triCnt (3*))
                                        (V.generate triCnt (\i -> i*3+1))
                                        (V.generate triCnt (\i -> i*3+2))



genSmoothings :: ( Show a, Epsilon a, Floating a, TangentableVertex pn txn nn a vn
                 , IElem (Position3 pn a) v, (v ++ '[Normal3 nn a]) ~ vn, (vn ++ '[Tangent3 tgn a]) ~ vnt)
              => Position3 pn a -> Texture2 txn a -> Normal3 nn a -> Tangent3 tgn a
              -> TriGeo (Vertex v) 
              -> TriGeo (Vertex vnt)
genSmoothings posF texF normF tanF = smoothTangents posF texF normF tanF . smoothNormals posF normF


{--
fakeTangents :: ( Epsilon a, Floating a, TangentableVertex pn txn nn a v, vt ~ (v ++ '[Tangent3 tgn a]) )
              => Position3 pn a -> Texture2 txn a -> Normal3 nn a -> Tangent3 tgn a
              -> TriGeo (Vertex v) -> TriGeo (Vertex vt)
fakeTangents posF texF normF tanF geo = geo { geoVertices = V.map (\v -> v <+> tanF =: V3 0 0 1) (geoVertices geo)}
--}


smoothNormals :: ( Show a, Epsilon a, Floating a, IElem (Position3 pn a) v, vn ~ (v ++ '[Normal3 nn a]))
              => Position3 pn a -> Normal3 nn a
              -> TriGeo (Vertex v) -> TriGeo (Vertex vn)
smoothNormals posF normF geo =
    let calcAvgNorm idx = averageNorm $ map (triangleUnnormal . fmap (rGet posF) . getTri verts) $ getShares geo idx
    in geo{ geoVertices = V.imap (\i v -> v <+> normF =: (calcAvgNorm i)) verts }
    where
    verts = geoVertices geo


type TangentableVertex pn txn nn a v = (IElem (Position3 pn a) v, IElem (Texture2 txn a) v, IElem (Normal3 nn a) v)
type PosAndTex pn txn a v = (IElem (Position3 pn a) v, IElem (Texture2 txn a) v)

smoothTangents :: ( Show a, Epsilon a, Floating a, TangentableVertex pn txn nn a v, vt ~ (v ++ '[Tangent3 tgn a]) )
              => Position3 pn a -> Texture2 txn a -> Normal3 nn a -> Tangent3 tgn a
              -> TriGeo (Vertex v) -> TriGeo (Vertex vt)
smoothTangents posF texF normF tanF geo =
    let calcTangentSpace' = calcTangentSpace posF texF normF 
    in geo { geoVertices = V.imap (\i v -> v <+> tanF =: ((calcTangentSpace' verts v i)^._y)) verts}
    where
    
    calcTangentSpace :: (Show a, Epsilon a, Floating a, TangentableVertex pn txn nn a v)
                 => Position3 pn a -> Texture2 txn a -> Normal3 nn a
                 -> V.Vector (Vertex v) 
                 -> Vertex v -> Int -> V3 (V3 a)
    calcTangentSpace posF texF normF vs v idx = 
        let (V2 t b) = fmap normalize . sum $ map (triSurfaceTangents posF texF normF . getTri vs) $ getShares geo idx
        in orthonormalize $ V3 (rGet normF v) t b

    triSurfaceTangents :: (Epsilon a, Floating a, TangentableVertex pn txn nn a v)
                        => Position3 pn a -> Texture2 txn a -> Normal3 nn a -> Triangle (Vertex v) -> V2 (V3 a)
    triSurfaceTangents posF texF _ tri =
        let Triangle p0 p1 p2 = rGet posF <$> tri
            Triangle (V2 s0 t0) (V2 s1 t1) (V2 s2 t2) = rGet texF <$> tri
            
            q1 = p1 - p0
            q2 = p2 - p0
            (s1', t1') = (s1 - s0, t1 - t0)
            (s2', t2') = (s2 - s0, t2 - t0)
            d  = 1 / (s1' * t2' - s2' * t1')
            tM = V2 (V2 t2' (-t1')) (V2 (-s2') (s1'))
            qM = V2 q1 q2
        in d *!! (tM !*! qM)

    verts = geoVertices geo

getTri :: V.Vector v -> Triangle Int -> Triangle v
getTri verts tri = (verts V.!) <$> tri


getShares :: Geometry v (Triangle Int) -> Int -> [Triangle Int]
getShares geo i = V.toList . V.filter (\(Triangle a b c) -> a == i || b == i || c == i) $ geoElements geo


instance (Binary v, Binary e) => Binary (Geometry v e)

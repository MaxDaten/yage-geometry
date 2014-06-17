{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Yage.Geometry
    ( module Yage.Geometry
    , module Elements
    ) where

import Yage.Prelude hiding (sum, toList, any, (++))
import Yage.Lens
import Yage.Math

import Control.Applicative (liftA3)
import Data.Binary
import Data.Foldable (any, toList)
import Data.Vector (Vector, (++))
import qualified Data.Vector as V
import qualified Data.Vector.Binary ()
import Control.DeepSeq
import Control.DeepSeq.Generics


import Yage.Geometry.Elements as Elements

type GeoSurface e = Vector e

data Geometry e v = Geometry
    { _geoVertices :: Vector v
    -- ^ all vertices of this geometry
    , _geoSurfaces :: Vector (GeoSurface e)
    -- ^ list of surfaces of the geometry. defined by objects (like Triangle Int) with indices to `geoVertices`
    } deriving ( Show, Eq, Ord, Functor, Foldable, Traversable, Generic )

makeLenses ''Geometry

type TriGeo = Geometry (Triangle Int)


-- | constructs a TriGeo from vertices, interpreted as triangles, as single surface and without reindexing
makeSimpleTriGeo :: Vector v -> TriGeo v
makeSimpleTriGeo verts = Geometry verts simpleIxs
    where
        triCnt    = V.length verts `div` 3 
        simpleIxs = V.singleton $ V.zipWith3 Triangle 
                                        (V.generate triCnt (3*))
                                        (V.generate triCnt (\i -> i*3+1))
                                        (V.generate triCnt (\i -> i*3+2))

-- | like `makeSimpleTriGeo` but extracts vertices from a `Foldable`
makeSimpleTriGeoF :: ( HasTriangles t, Foldable f ) => f (t v) -> TriGeo v
makeSimpleTriGeoF = makeSimpleTriGeo . V.concatMap (V.fromList . vertices) . V.map triangles . V.fromList . toList


empty :: Geometry e v
empty = Geometry V.empty V.empty

{--
indexedSurface :: Eq v => Surface (Triangle v) -> TriGeo v
indexedSurface triSurf = 
    let surfVec = V.fromList $ nub $ concatMap vertices $ getSurface triSurf
    in Geometry { geoVerices  = undefined
                , geoElements = undefined
                }
--}


type Pos       = V3
type Normal    = V3
type Tex       = V2
type TBN a     = M33 a


-- | calc tangent spaces for each triangle. averages for normals and tangents are calculated on surfaces
calcTangentSpaces :: ( Epsilon a, Floating a, Show a ) => 
    TriGeo (Pos a) ->
    TriGeo (Tex a) -> 
    TriGeo (TBN a)
calcTangentSpaces posGeo texGeo = 
    calcTangentSpaces' posGeo texGeo $ calcNormals posGeo



calcNormals :: ( Epsilon a, Floating a, Show a )
            => TriGeo (Pos a) -> TriGeo (Normal a)
calcNormals geo = traceShowS' "x" $ uncurry Geometry normalsOverSurfaces 
    where
    normalsOverSurfaces = V.foldl' normalsForSurface (V.empty, V.empty) (geo^.geoSurfaces)
    
    normalsForSurface (normsAccum, surfacesAccum) surface = 
        let (normVerts, normedSurface) = V.foldl' (normalsForTriangle surface) (normsAccum, V.empty) surface
        in (normVerts, surfacesAccum `V.snoc` normedSurface )
    
    normalsForTriangle inSurface (vertsAccum, surfaceAccum) triangle =
        let normedTri = fmap (calcAvgNorm inSurface) triangle
            idx       = V.length vertsAccum
            idxTri    = Triangle idx (idx + 1) (idx + 2)
        in vertsAccum   `seq`
           surfaceAccum `seq`
           (vertsAccum ++ (V.fromList . toList $ normedTri), surfaceAccum `V.snoc` idxTri)

    posVerts                = geo^.geoVertices
    calcAvgNorm surface idx = averageNorm $ V.map (triangleUnnormal . toPosTri) $ getShares idx surface
    toPosTri                = fmap (posVerts V.!)



calcTangentSpaces' :: forall a. ( Epsilon a, Floating a, Show a) =>
    TriGeo (Pos a) ->
    TriGeo (Tex a) ->
    TriGeo (Normal a) -> 
    TriGeo (TBN a)
calcTangentSpaces' posGeo texGeo normGeo
    | not compatibleSurfaces = error "calcTangentSpaces': surfaces doesn't match"
    | otherwise = traceShowS' "y" $ uncurry Geometry tbnOverSurfaces


    where
    tbnOverSurfaces = V.foldl' tbnForSurface (V.empty, V.empty) pntIdxs
    
    tbnForSurface (tbnAccum, surfacesAccum) surface = 
        let (tbnVerts, tbnSurface) = V.foldl' (tbnForTriangle surface) (tbnAccum, V.empty) surface
        in tbnVerts      `seq`
           surfacesAccum `seq`
           (tbnVerts, surfacesAccum `V.snoc` tbnSurface )

    tbnForTriangle inSurface (vertsAccum, surfaceAccum) triangle = 
        let tbnTriangle = fmap (calcTangentSpace inSurface) triangle
            idx         = V.length vertsAccum
            idxTri      = Triangle idx (idx + 1) (idx + 2)
        in vertsAccum   `seq` 
           surfaceAccum `seq` 
           (vertsAccum ++ (V.fromList . toList $ tbnTriangle), surfaceAccum `V.snoc` idxTri )

    pntIdxs :: Vector (GeoSurface (Triangle (Int, Int, Int)))
    pntIdxs = traceShowId $ V.zipWith3 (V.zipWith3 (liftA3 (,,))) (traceShowId $ posGeo^.geoSurfaces) (traceShowId $ normGeo^.geoSurfaces) (traceShowId $ texGeo^.geoSurfaces)

    toPNTTri :: ( Epsilon a, Floating a) => Triangle (Int, Int, Int) -> (Triangle (Pos a), Triangle (Normal a), Triangle (Tex a))
    toPNTTri tri = ( V.unsafeIndex (posGeo^.geoVertices)  . (^._1) <$> tri
                   , V.unsafeIndex (normGeo^.geoVertices) . (^._2) <$> tri
                   , V.unsafeIndex (texGeo^.geoVertices)  . (^._3) <$> tri
                   ) 

    calcTangentSpace :: ( Epsilon a, Floating a) => GeoSurface (Triangle (Int, Int, Int)) -> (Int, Int, Int) -> M33 a
    calcTangentSpace surface (posIdx, normIdx, _texIdx) =
        let normal          = V.unsafeIndex (normGeo^.geoVertices) normIdx
            toPTTri (p,_,t) = (p,t)
            sharePosIdx :: Int -> Triangle (Int, Int, Int) -> Bool
            sharePosIdx i   = any (\(p,_,_) -> p==i)
            ~(V3 t b _n)    = V.sum $ V.map (uncurry triangleTangentSpace . toPTTri . toPNTTri) $ V.filter (sharePosIdx posIdx) surface
        in orthonormalize $ V3 t b normal

    compatibleSurfaces = 
        let posSurfaces  = traceShowS' "y" $ posGeo^.geoSurfaces^..traverse.to length
            texSurfaces  = traceShowS' "y" $ texGeo^.geoSurfaces^..traverse.to length
            normSurfaces = traceShowS' "y " $ normGeo^.geoSurfaces^..traverse.to length
        in posSurfaces == texSurfaces && posSurfaces == normSurfaces
{--
    normGeo{ geoVertices = V.imap calcTangentSpace ( geoVertices normGeo ) }
    where
    
    calcTangentSpace i n =
        let ~(V3 t b _n) = V.sum $ V.map (uncurry triangleTangentSpace . toNormTexTri) $ V.filter (shareNormalIndex i) normalWithTexIdx
        in orthonormalize $ V3 t b n


    shareNormalIndex :: Int -> Triangle (Int, Int) -> Bool
    shareNormalIndex i = any ((==i) . fst)
    
    normalWithTexIdx :: V.Vector (Triangle (Int, Int))
    normalWithTexIdx = V.zipWith (liftA2 (,)) (geoElements normGeo) (geoElements texGeo)
--}

getShares :: Int -> Vector (Triangle Int) -> Vector (Triangle Int)
getShares i = V.filter (any (==i))



instance (Binary e, Binary v) => Binary (Geometry e v)

instance (NFData e, NFData v) => NFData (Geometry e v) where rnf = genericRnf


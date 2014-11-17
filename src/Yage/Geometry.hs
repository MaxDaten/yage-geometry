{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Yage.Geometry
    ( module Yage.Geometry
    , module Elements
    ) where

import           Yage.Lens
import           Yage.Math
import           Yage.Prelude           hiding (any, sum, toList, (++))

import           Control.Applicative    (liftA3)
import           Data.Binary
import           Data.Foldable          (any, toList)
import           Data.Vector            ((++))
import qualified Data.Vector            as V
import qualified Data.Vector.Binary     ()

import           Yage.Geometry.Elements as Elements

-- wrapping neccassry because of ghc bug
-- https://github.com/bos/vector-binary-instances/issues/4
newtype GeoSurface e = GeoSurface { unGeoSurface :: Vector e }
    deriving ( Show, Eq, Ord, Functor, Foldable, Traversable, Generic )

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
        simpleIxs = V.singleton . GeoSurface $ V.zipWith3 Triangle
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
calcTangentSpaces :: ( Epsilon a, Floating a ) =>
    TriGeo (Pos a) ->
    TriGeo (Tex a) ->
    TriGeo (TBN a)
calcTangentSpaces posGeo texGeo =
    calcTangentSpaces' posGeo texGeo $ calcNormals posGeo



calcNormals :: ( Epsilon a, Floating a )
            => TriGeo (Pos a) -> TriGeo (Normal a)
calcNormals geo = uncurry Geometry normalsOverSurfaces
    where
    normalsOverSurfaces = V.foldl' normalsForSurface (V.empty, V.empty) (geo^.geoSurfaces)

    normalsForSurface (normsAccum, surfacesAccum) (GeoSurface surface) =
        let (normVerts, normedSurface) = V.foldl' (normalsForTriangle surface) (normsAccum, V.empty) surface
        in (normVerts, surfacesAccum `V.snoc` (GeoSurface normedSurface) )

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



calcTangentSpaces' :: forall a. ( Epsilon a, Floating a ) =>
    TriGeo (Pos a) ->
    TriGeo (Tex a) ->
    TriGeo (Normal a) ->
    TriGeo (TBN a)
calcTangentSpaces' posGeo texGeo normGeo
    | not compatibleSurfaces = error "calcTangentSpaces': surfaces doesn't match"
    | otherwise = uncurry Geometry tbnOverSurfaces


    where
    tbnOverSurfaces = V.foldl' tbnForSurface (V.empty, V.empty) pntIdxs

    tbnForSurface (tbnAccum, surfacesAccum) (GeoSurface geoSurface) =
        let (tbnVerts, tbnSurface) = V.foldl' (tbnForTriangle geoSurface) (tbnAccum, V.empty) geoSurface
        in tbnVerts      `seq`
           surfacesAccum `seq`
           (tbnVerts, surfacesAccum `V.snoc` (GeoSurface tbnSurface) )

    tbnForTriangle inSurface (vertsAccum, surfaceAccum) triangle =
        let tbnTriangle = fmap (calcTangentSpace inSurface) triangle
            idx         = V.length vertsAccum
            idxTri      = Triangle idx (idx + 1) (idx + 2)
        in vertsAccum   `seq`
           surfaceAccum `seq`
           (vertsAccum ++ (V.fromList . toList $ tbnTriangle), surfaceAccum `V.snoc` idxTri )

    pntIdxs :: Vector (GeoSurface (Triangle (Int, Int, Int)))
    pntIdxs =
        let mkSurface (GeoSurface p) (GeoSurface n) (GeoSurface t) = GeoSurface $ V.zipWith3 (liftA3 (,,)) p n t
        in V.zipWith3 mkSurface (posGeo^.geoSurfaces) ( normGeo^.geoSurfaces) ( texGeo^.geoSurfaces)

    toPNTTri :: ( Epsilon a, Floating a) => Triangle (Int, Int, Int) -> (Triangle (Pos a), Triangle (Normal a), Triangle (Tex a))
    toPNTTri tri = ( (V.!) (posGeo^.geoVertices)  . (^._1) <$> tri
                   , (V.!) (normGeo^.geoVertices) . (^._2) <$> tri
                   , (V.!) (texGeo^.geoVertices)  . (^._3) <$> tri
                   )

    calcTangentSpace :: ( Epsilon a, Floating a) => V.Vector (Triangle (Int, Int, Int)) -> (Int, Int, Int) -> M33 a
    calcTangentSpace geoSurface (posIdx, normIdx, _texIdx) =
        let normal          = V.unsafeIndex (normGeo^.geoVertices) normIdx
            toPTTri (p,_,t) = (p,t)
            sharePosIdx :: Int -> Triangle (Int, Int, Int) -> Bool
            sharePosIdx i   = any (\(p,_,_) -> p==i)
            ~(V3 t b _n)    = V.sum $ V.map (uncurry triangleTangentSpace . toPTTri . toPNTTri) $ V.filter (sharePosIdx posIdx) geoSurface
        in orthonormalize $ V3 t b normal

    compatibleSurfaces =
        let posSurfaces  = posGeo^.geoSurfaces^..traverse.to (length.unGeoSurface)
            texSurfaces  = texGeo^.geoSurfaces^..traverse.to (length.unGeoSurface)
            normSurfaces = normGeo^.geoSurfaces^..traverse.to (length.unGeoSurface)
        in posSurfaces == texSurfaces && posSurfaces == normSurfaces


getShares :: Int -> Vector (Triangle Int) -> Vector (Triangle Int)
getShares i = V.filter (any (==i))



instance Binary e => Binary (GeoSurface e)
instance (Binary e, Binary v) => Binary (Geometry e v)

instance (NFData e) => NFData (GeoSurface e) where rnf = genericRnf
instance (NFData e, NFData v) => NFData (Geometry e v) where rnf = genericRnf


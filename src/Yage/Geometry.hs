{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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

import Yage.Prelude hiding (sum, toList, any)
import Yage.Math

import Control.Applicative (liftA2)
import Data.Binary
import Data.Foldable (any, toList)
import qualified Data.Vector as V
import qualified Data.Vector.Binary ()

import Yage.Geometry.Elements as Elements


data Geometry e v = Geometry
    { geoVertices :: V.Vector v
    , geoElements :: V.Vector e     -- | like Triangle i, where i is the index to a vertex
    } deriving ( Show, Eq, Ord, Functor, Foldable, Traversable, Generic )

type TriGeo = Geometry (Triangle Int)

-- | constructs a Geo from vertices, interpreted as triangles and without reindexing
makeSimpleTriGeo :: V.Vector v -> TriGeo v
makeSimpleTriGeo verts = Geometry verts simpleIxs
    where
        triCnt = V.length verts `div` 3 
        simpleIxs = V.zipWith3 Triangle (V.generate triCnt (3*))
                                        (V.generate triCnt (\i -> i*3+1))
                                        (V.generate triCnt (\i -> i*3+2))

makeSimpleTriGeo' :: ( HasTriangles t, Foldable f ) => f (t v) -> TriGeo v
makeSimpleTriGeo' = makeSimpleTriGeo . V.concatMap (V.fromList . vertices) . V.map triangles . V.fromList . toList

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


calcTangentSpaces :: ( Epsilon a, Floating a ) => 
              TriGeo (Pos a) ->
              TriGeo (Tex a) -> 
              TriGeo (TBN a)
calcTangentSpaces posGeo texGeo = calcTangentSpaces' posGeo texGeo $ calcNormals posGeo



calcNormals :: ( Epsilon a, Floating a )
            => TriGeo (Pos a) -> TriGeo (Normal a)
calcNormals geo =
    geo{ geoVertices = V.imap (\i _ -> calcAvgNorm i) verts }
    where
    verts           = geoVertices geo
    calcAvgNorm idx = averageNorm $ map (triangleUnnormal . toPosTri) $ getShares idx (geoElements geo)
    toPosTri = fmap (verts V.!)



calcTangentSpaces' :: ( Epsilon a, Floating a)
               => TriGeo (Pos a)
               -> TriGeo (Tex a)
               -> TriGeo (Normal a)
               -> TriGeo (TBN a)
calcTangentSpaces' posGeo texGeo normGeo =
    normGeo{ geoVertices = V.imap calcTangentSpace ( geoVertices normGeo ) }
    where
    
    calcTangentSpace i n =
        let ~(V3 t b _n) = V.sum $ V.map (uncurry triangleTangentSpace . toNormTexTri) $ V.filter (shareNormalIndex i) normalWithTexIdx
        in orthonormalize $ V3 t b n

    toNormTexTri tri = (V.unsafeIndex (geoVertices posGeo) . fst <$> tri, V.unsafeIndex (geoVertices texGeo) . snd <$> tri) 

    shareNormalIndex :: Int -> Triangle (Int, Int) -> Bool
    shareNormalIndex i = any ((==i) . fst)
    
    normalWithTexIdx :: V.Vector (Triangle (Int, Int))
    normalWithTexIdx = V.zipWith (liftA2 (,)) (geoElements normGeo) (geoElements texGeo)

getShares :: Int -> V.Vector (Triangle Int) -> [Triangle Int]
getShares i = V.toList . V.filter (any (==i))



instance (Binary v, Binary e) => Binary (Geometry v e)

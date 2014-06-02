{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TupleSections  #-}

module Yage.Geometry
    ( module Yage.Geometry
    , module Elements
    ) where

import Yage.Prelude hiding (sum, toList, any)
import Yage.Lens
import Yage.Math

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
        let normIndexFilter = any ((==i) . snd)
            (V2 t b)        = V.sum $ V.map (triTangentSpace . toNormTexTri) $ V.filter normIndexFilter texNormIdx
        in orthonormalize $ V3 t b n

    triTangentSpace tri =
        let Triangle pos0 pos1 pos2 = fmap snd tri
            Triangle st0 st1 st2    = fmap fst tri
            
            deltaPos1 = pos1 - pos0
            deltaPos2 = pos2 - pos0
            deltaUV1  = st1 - st0
            deltaUV2  = st2 - st0
            d = 1.0 / ( deltaUV1^._x * deltaUV2^._y - deltaUV1^._y * deltaUV2^._x )
            t = ( deltaPos1 ^* ( deltaUV2^._y ) - deltaPos2 ^* ( deltaUV1^._y ) ) ^* d
            b = ( deltaPos2 ^* ( deltaUV1^._x ) - deltaPos1 ^* ( deltaUV2^._x ) ) ^* d
        in V2 t b

    toNormTexTri = fmap (\(ti, ni) -> ((geoVertices texGeo) V.! ti, (geoVertices posGeo) V.! ni))
    texNormIdx = V.zipWith (\triT triN -> (,) <$> triT <*> triN) (geoElements texGeo) (geoElements normGeo)

getShares :: Int -> V.Vector (Triangle Int) -> [Triangle Int]
getShares i = V.toList . V.filter (any (==i))



instance (Binary v, Binary e) => Binary (Geometry v e)

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
import Yage.Math
import Yage.Lens

import Data.Vinyl
import Data.Binary
import Data.Foldable (toList, any)
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

type Tangent = V3
type Pos = V3
type Normal = V3
type Tex = V2
type NT a = (Normal a, Tangent a)

genSmoothings :: ( Show a, Epsilon a, Floating a)
              => TriGeo (Pos a) -> TriGeo (Tex a)
              -> TriGeo (NT a)
genSmoothings posGeo texGeo = smoothTangents posGeo texGeo $ smoothNormals posGeo


{--
fakeTangents :: ( Epsilon a, Floating a )
              => TriGeo (Tex a) -> TriGeo (Normal a) -> TriGeo (NT a)
fakeTangents texGeo normGeo = normGeo { geoVertices = V.map ((, (V3 0 0 1)))  (geoVertices normGeo) }
--}

smoothNormals :: ( Show a, Epsilon a, Floating a)
              => TriGeo (Pos a) -> TriGeo (Normal a)
smoothNormals geo =
    geo{ geoVertices = V.imap (\i _ -> calcAvgNorm i) verts }
    where
    verts           = geoVertices geo
    calcAvgNorm idx = averageNorm $ map (triangleUnnormal . toPosTri) $ getShares idx (geoElements geo)
    toPosTri =  fmap (verts V.!)



smoothTangents :: ( Epsilon a, Floating a)
               => TriGeo (Pos a)
               -> TriGeo (Tex a)
               -> TriGeo (Normal a)
               -> TriGeo (NT a)
smoothTangents posGeo texGeo normGeo =
    normGeo { geoVertices = V.imap (\i n -> (n, (calcTangentSpace n i)^._y)) (geoVertices normGeo) }
    where
    
    calcTangentSpace n i = 
        let normIndexFilter = any ((==i) . snd)
            (V2 t b) = V.sum $ V.map (triSurfaceTangents . toNormTexTri) $ V.filter normIndexFilter texNormIdx
        in orthonormalize $ V3 n t b

    triSurfaceTangents tri =
        let Triangle p0 p1 p2 = fmap snd tri
            Triangle (V2 s0 t0) (V2 s1 t1) (V2 s2 t2) = fmap fst tri
            
            q1 = p1 - p0
            q2 = p2 - p0
            (s1', t1') = (s1 - s0, t1 - t0)
            (s2', t2') = (s2 - s0, t2 - t0)
            d  = 1 / (s1' * t2' - s2' * t1')
            tM = V2 (V2 t2' (-t1')) (V2 (-s2') (s1'))
            qM = V2 q1 q2
        in d *!! tM !*! qM

    toNormTexTri = fmap (\(ti, ni) -> ((geoVertices texGeo) V.! ti, (geoVertices posGeo) V.! ni))
    texNormIdx = V.zipWith (\triT triN -> (,) <$> triT <*> triN) (geoElements texGeo) (geoElements normGeo)
{--
type TangentableVertex pn txn nn a v = (IElem (Position3 pn a) v, IElem (Texture2 txn a) v, IElem (Normal3 nn a) v)
type PosAndTex pn txn a v = (IElem (Position3 pn a) v, IElem (Texture2 txn a) v)

--}

getShares :: Int -> V.Vector (Triangle Int) -> [Triangle Int]
getShares i = V.toList . V.filter (any (==i))

packGeos :: (Epsilon a, Floating a) 
         => (Pos a -> Tex a -> NT a -> v) -> TriGeo (Pos a) -> TriGeo (Tex a) -> TriGeo (NT a) -> TriGeo v
packGeos format posG texG ntG 
    | sameLength = error "can't merge geos, invalid number of elements"
    | otherwise =
        let mergedIdxs = V.zipWith3 mergeIndices (geoElements posG) (geoElements texG) (geoElements ntG) :: V.Vector (Triangle (Int, Int, Int))
            vertices  = V.concatMap (V.fromList . toList . (fmap emitVertex)) mergedIdxs
        in Geometry{geoVertices = vertices, geoElements = V.generate (length mergedIdxs) (\i -> Triangle (i*3) (i*3+1) (i*3+2))}
    where

    emitVertex (vertIdx, texIdx, ntIdx) =
        format (verts V.! vertIdx) (texs V.! texIdx) (norms V.! ntIdx)

    mergeIndices p tx tn  = (,,) <$> p <*> tx <*> tn

    verts = geoVertices posG
    texs = geoVertices texG
    norms = geoVertices ntG

    sameLength =
        let lp = length $ geoElements posG
            lt = length $ geoElements texG
            ln = length $ geoElements ntG
        in lp /= lt || ln /= lt || lp /= ln



instance (Binary v, Binary e) => Binary (Geometry v e)

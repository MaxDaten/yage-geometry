{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main, spec) where

import Yage.Prelude
import Yage.Math hiding (point)

import Test.Hspec

import Data.Vinyl.Instances ()
import Data.Binary
import qualified Data.Vector as V 
import Yage.Geometry
import Yage.Geometry.Vertex

import System.Random
import System.Directory

main :: IO ()
main = hspec spec

type VT = P3T2N3 "pos" "tex" "norm" Float

point :: Point (Vertex VT)
point = Point $ position3 =: V3 1 2 3
             <+> texture2 =: V2 1 0
             <+> normal3  =: V3 0 1 0 

--addTangents :: Geometry (Vertex VT) (Triangle Int) -> Geometry (Vertex VTT) (Triangle Int)
-- addTangents = smoothTangents position3 texture2 normal3 tangent3

spec :: Spec
spec = do
--{--
  describe "Binary Elements" $ do
    it "writes Point with Binary.encode and decodes it back" $ do
      (decode . encode $ point) `shouldBe` point

    it "writes Triangle with Binary.encode and decodes it back" $ do
      let triangle = Triangle 10 11 12 :: Triangle Int 
      (decode . encode $ triangle) `shouldBe` triangle

    it "writes a Vector (Triangle Int) with Binary.encode and decodes it back" $ do
      trianglesV <- V.fromList <$> genIdxTris 10 99
      (decode . encode $ trianglesV) `shouldBe` trianglesV
  
    -- test-case for https://github.com/bos/vector-binary-instances/issues/4
    {--
    it "writes a nested Vector( Vector (Triangle Int) ) with Binary.encode and decodes it back" $ do
      trianglesVV <- V.fromList <$> replicateM 10 (V.fromList <$> genIdxTris 10 99)
      (decode . encode $ trianglesVV) `shouldBe` trianglesVV
    --}

--{--
  
  describe "Binary Geometry" $ do
    it "writes Geometry.empty with Binary.encode and decodes it back" $ do
      let e = empty :: Geometry (Vertex VT) (Triangle Int)
      (decode . encode $ e) `shouldBe` e

    it "writes Geo to Binary and reads it back" $ do
      verts   <- genVertices 100
      tris    <- replicateM 10 $ genIdxTris 10 99
      let geo = Geometry (V.fromList verts) (V.fromList ( map (GeoSurface .V.fromList) tris))
      (decode . encode $ geo) `shouldBe` geo

    it "writes Geo to Binary file and reads it back" $ do
      verts   <- genVertices 100
      tris    <- replicateM 10 $ genIdxTris 10 99
      let file = "geo.tmp"
          geo  = Geometry (V.fromList verts) (V.fromList (map (GeoSurface . V.fromList) tris)) 
      encodeFile file geo
      fileGeo <- decodeFile file  
      fileGeo `shouldBe` geo
      removeFile file

--}
{--

  describe "Binary YGM" $ do
    it "writes YGM to Binary file and reads it back" $ do
      verts   <- genVertices 100
      tris    <- genIdxTris 100 99
      let file = "ygm.tmp"
          geo  = Geometry (V.fromList verts) (V.fromList tris) 
      encodeFile file $ YGM "somename" geo
      fileYGM <- decodeFile file
      ygmModel fileYGM `shouldBe` geo
      ygmName fileYGM `shouldBe` "somename"
      removeFile file

--}
{--

  describe "Binary YGM" $ do
    it "write YGM with generated tangents to Binary file and read it back" $ do
      verts   <- genVertices 100
      tris    <- genIdxTris 100 99
      let file = "ygm.tmp"
          geo  = addTangents $ Geometry (V.fromList verts) (V.fromList tris) 
      encodeFile file $ YGM "somename" geo
      fileYGM <- decodeFile file
      ygmModel fileYGM `shouldBe` geo
      ygmName fileYGM `shouldBe` "somename"
      removeFile file

--}

genVertices :: Int -> IO ([Vertex VT])
genVertices cnt = replicateM cnt randomVertex
  where
    randomVertex = do
        [p1, p2, p3] <- replicateM 3 randomIO
        [n1, n2, n3] <- replicateM 3 randomIO
        [t1, t2]     <- replicateM 2 randomIO
        return ( position3    =: V3 p1 p2 p3
             <+> texture2     =: V2 t1 t2
             <+> normal3      =: V3 n1 n2 n3
               )

genIdxTris :: Int -> Int -> IO ([Triangle Int])
genIdxTris cnt maxIx = 
  replicateM cnt $ Triangle <$> randomRIO (0, maxIx)
                            <*> randomRIO (0, maxIx)
                            <*> randomRIO (0, maxIx)
    

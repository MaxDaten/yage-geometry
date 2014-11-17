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

import System.Random
import System.Directory

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
--{--
  describe "Binary Elements" $ do
    it "writes Point with Binary.encode and decodes it back" $ do
      let point = Point 42 :: Point (Int)
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
      let e = empty :: Geometry (V3 Float) (Triangle Int)
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


genVertices :: Int -> IO ([V3 Float])
genVertices cnt = replicateM cnt $ V3 <$> randomIO <*> randomIO <*> randomIO

genIdxTris :: Int -> Int -> IO ([Triangle Int])
genIdxTris cnt maxIx =
  replicateM cnt $ Triangle <$> randomRIO (0, maxIx)
                            <*> randomRIO (0, maxIx)
                            <*> randomRIO (0, maxIx)


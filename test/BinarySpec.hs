{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main, spec) where

import Yage.Prelude
import Yage.Math hiding (point)

import Test.Hspec
--import Test.QuickCheck

import Data.Vinyl.Binary ()
import Data.Binary
import qualified Data.Vector as V 
import Yage.Geometry
import Yage.Geometry.Vertex
import Yage.Geometry.Formats.Ygm

import System.Random
import System.Directory

main :: IO ()
main = hspec spec

type VT = P3N3T2 "pos" "norm" "tex" Float
point :: Point (Vertex VT)
point = Point $ position3 =: V3 1 2 3
             <+> normal3  =: V3 0 1 0 
             <+> texture2 =: V2 1 0

spec :: Spec
spec = do
  describe "Binary Point" $ do
    it "write Point to Binary and read it back" $ do
      (decode . encode $ point) `shouldBe` point
  
  describe "Binary Geometry" $ do
    it "write Geo to Binary and read it back" $ do
      geo <- Geometry . V.fromList <$> genVerts 100
      (decode . encode $ geo) `shouldBe` geo
  
  describe "Binary Geometry" $ do
    it "write Geo to Binary file and read it back" $ do
      let file = "geo.tmp"
      geo <- Geometry . V.fromList <$> genVerts 100
      encodeFile file geo
      fileGeo <- decodeFile file  
      fileGeo `shouldBe` geo
      removeFile file
  
  describe "Binary YGM" $ do
    it "write YGM to Binary file and read it back" $ do
      let file = "ygm.tmp"
      geo <- Geometry . V.fromList <$> genVerts 100
      encodeFile file $ YGM "somename" geo
      fileYGM <- decodeFile file
      ygmModel fileYGM `shouldBe` geo
      ygmName fileYGM `shouldBe` "somename"
      removeFile file



genVerts :: Int -> IO ([Triangle (Vertex VT)])
genVerts cnt = replicateM cnt (Triangle <$> randomVertex
                                        <*> randomVertex
                                        <*> randomVertex)
    where
    randomVertex = do
        [p1, p2, p3] <- replicateM 3 randomIO
        [n1, n2, n3] <- replicateM 3 randomIO
        [t1, t2]     <- replicateM 2 randomIO
        return ( position3    =: V3 p1 p2 p3
             <+> normal3      =: V3 n1 n2 n3
             <+> texture2     =: V2 t1 t2
               )

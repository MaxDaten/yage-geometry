{-# LANGUAGE BangPatterns #-}
module Main where

import           Linear.DeepSeq ()
import           Yage.Math
import           Yage.Prelude

import           Criterion.Main
import           Data.Binary
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           System.Random

import           Yage.Geometry.Elements


type VL = LByteString -> [Triangle (V3 Float)]
type VVec = LByteString -> Vector (Triangle (V3 Float))

main :: IO ()
main = do
    !triList <- force <$> genTrianglesV3 (10^3 :: Int)
    let !triVec = force (V.fromList triList)

    defaultMain [
        bgroup "encode Triangle"
            [ bench "Triangle List"   $ nf encode triList
            , bench "Triangle Vector" $ nf encode triVec
            ],
        bgroup "decode Triangle"
            [ bench "Triangle List"   $ nf (decode :: VL ) (encode triList)
            , bench "Triangle Vector" $ nf (decode :: VVec) (encode triVec)
            ]
        ]




genTrianglesV3 :: Int -> IO ([Triangle (V3 Double)])
genTrianglesV3 cnt = replicateM cnt $ Triangle <$> randomV3 <*> randomV3 <*> randomV3

randomV3 :: Random a => IO (V3 a)
randomV3 = V3 <$> randomIO <*> randomIO <*> randomIO


{-# LANGUAGE OverloadedStrings #-}
module Main (main, spec) where

import Yage.Prelude
import Yage.Math
import Yage.Lens hiding (elements)

import qualified Data.Vector as V
import Test.Hspec
--import Test.QuickCheck hiding (elements)

import Yage.Geometry.Formats.Obj.Parser

main :: IO ()
main = hspec spec

squareOBJ :: OBJ
squareOBJ = mempty & vertexData.geometricVertices .~ V.fromList [ V3 0 2 0, V3 0 0 0, V3 2 0 0, V3 2 2 0 ]
                   & vertexData.vertexNormals     .~ V.fromList [ V3 0 0 1 ]
                   & vertexData.textureVertices   .~ V.fromList [ V2 0 0  , V2 0 1  , V2 1 0  , V2 1 1 ]
                   & elements.faces .~ V.singleton [[ VertexIndex 1, TextureIndex 1, NormalIndex 1 ]
                                                   ,[ VertexIndex 2, TextureIndex 2, NormalIndex 1 ]
                                                   ,[ VertexIndex 3, TextureIndex 3, NormalIndex 1 ]
                                                   ,[ VertexIndex 4, TextureIndex 4, NormalIndex 1 ]
                                                   ]

spec :: Spec
spec = do
  describe "parseOBJFile" $ do
    it "parses a simple square" $ do
      parsedObj <- parseOBJFile $ "test" </> "res" </> "square.obj"
      parsedObj `shouldBe` squareOBJ

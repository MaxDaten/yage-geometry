{-# LANGUAGE OverloadedStrings #-}
module Main where

import Yage.Prelude

import Test.Hspec

import Test.Yage.Geometry.D2.Rectangle
import Test.Yage.Geometry.D2.Bound


main :: IO ()
main = hspec $ do
    boundSpec
    
    rectangleSpec

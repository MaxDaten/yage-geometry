{-# LANGUAGE OverloadedStrings #-}
module Main where

import Yage.Prelude

import Test.Hspec

import Test.Yage.Geometry.D2.Rectangle


main :: IO ()
main = hspec $ do
    rectangleSpec

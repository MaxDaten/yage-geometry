{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell    #-}
module Test.Yage.Geometry.D2.Bound where

import Yage.Prelude
import Yage.Math

import Test.Hspec

import Yage.Geometry.D2.Bound

boundSpec :: Spec
boundSpec = do
    

    describe "area on a Bound" $ do
        it "calculates the correct area" $ do
            area (Bound 0 (V2 3 5)) `shouldBe` 15

    
    describe "Bounds fits" $ do
        it "if the extends of the first are smaller then the seconds extends, regardless of their centers" $ do 
            (Bound 0 1 `fits` Bound 2 2) `shouldBe` True

        it "not if the extends of first are bigger then the seconds extends, regardless of their centers" $ do
            (Bound 2 2 `fits` Bound 0 0) `shouldBe` False

        it "not if the areas are equal but have different alignments" $ do
            (Bound 0 (V2 2 3) `fits` Bound 0 (V2 3 2)) `shouldBe` False

        it "if the area and alignments are equal" $ do
            (Bound 0 (V2 2 3) `fits` Bound 0 (V2 2 3)) `shouldBe` True

    
    describe "A point (V2) is in a Bound" $ do
        it "if the point equals the center of the Bound" $ do
            ((Bound (V2 2 3) 3) `containsPoint` V2 2 3) `shouldBe` True

        it "if the point lies on border" $ do
            ((Bound (V2 2 3) 2) `containsPoint` V2 3 4) `shouldBe` True


    describe "A point (V2) is NOT in a Bound" $ do
        it "if the point is outside on one axis" $ do
            ((Bound (V2 2 3) 2) `containsPoint` V2 4 4) `shouldBe` False

        it "if the point is outside on both axis" $ do
            ((Bound (V2 2 3) 2) `containsPoint` V2 4 5) `shouldBe` False


    describe "Bound A contains Bound B completly" $ do
        it "if their centers are equal and B is smaller" $ do
            (Bound 0 3 `containsBound` Bound 0 2) `shouldBe` True
        
        it "if A and B are equal" $ do
            (Bound 0 3 `containsBound` Bound 0 3) `shouldBe` True

        it "if A is bigger and B smaller, but touches border on both axes" $ do
            (Bound 0 4 `containsBound` Bound 1 2) `shouldBe` True


    describe "Bound A contains Bound B NOT completly" $ do
        it "if their centers are equal but B is bigger" $ do
            (Bound 0 2 `containsBound` Bound 0 3) `shouldBe` False
        
        it "if the extends of A and B are equal but their centers not" $ do
            (Bound 0 3 `containsBound` Bound 1 3) `shouldBe` False

        it "if B is completely somewhere else" $ do
            (Bound 0 2 `containsBound` Bound 3 2) `shouldBe` False


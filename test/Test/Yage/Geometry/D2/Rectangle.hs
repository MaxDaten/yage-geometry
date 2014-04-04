{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell    #-}
module Test.Yage.Geometry.D2.Rectangle where

import Yage.Prelude
import Yage.Math

import Test.Hspec

import Yage.Geometry.D2.Rectangle

rectangleSpec :: Spec
rectangleSpec = do
    

    describe "area on a Rectangle" $ do
        it "calculates the correct area" $ do
            area (Rectangle 0 (V2 3 5)) `shouldBe` 15

    
    describe "Rectangles fits" $ do
        it "if the extends of the first are smaller then the seconds extends, regardless of their centers" $ do 
            (Rectangle 0 1 `fits` Rectangle 2 2) `shouldBe` True

        it "not if the extends of first are bigger then the seconds extends, regardless of their centers" $ do
            (Rectangle 2 2 `fits` Rectangle 0 0) `shouldBe` False

        it "not if the areas are equal but have different alignments" $ do
            (Rectangle 0 (V2 2 3) `fits` Rectangle 0 (V2 3 2)) `shouldBe` False

        it "if the area and alignments are equal" $ do
            (Rectangle 0 (V2 2 3) `fits` Rectangle 0 (V2 2 3)) `shouldBe` True

    
    describe "A point (V2) is in a Rectangle" $ do
        it "if the point equals the center of the Rectangle" $ do
            ((Rectangle (V2 2 3) 3) `containsPoint` V2 2 3) `shouldBe` True

        it "if the point lies on border" $ do
            ((Rectangle (V2 2 3) 2) `containsPoint` V2 3 4) `shouldBe` True


    describe "A point (V2) is NOT in a Rectangle" $ do
        it "if the point is outside on one axis" $ do
            ((Rectangle (V2 2 3) 2) `containsPoint` V2 4 4) `shouldBe` False

        it "if the point is outside on both axis" $ do
            ((Rectangle (V2 2 3) 2) `containsPoint` V2 4 5) `shouldBe` False


    describe "Rectangle A contains Rectangle B completly" $ do
        it "if their centers are equal and B is smaller" $ do
            (Rectangle 0 3 `containsRectangle` Rectangle 0 2) `shouldBe` True
        
        it "if A and B are equal" $ do
            (Rectangle 0 3 `containsRectangle` Rectangle 0 3) `shouldBe` True

        it "if A is bigger and B smaller, but touches border on both axes" $ do
            (Rectangle 0 4 `containsRectangle` Rectangle 1 2) `shouldBe` True


    describe "Rectangle A contains Rectangle B NOT completly" $ do
        it "if their centers are equal but B is bigger" $ do
            (Rectangle 0 2 `containsRectangle` Rectangle 0 3) `shouldBe` True
        
        it "if the extends of A and B are equal but their centers not" $ do
            (Rectangle 0 3 `containsRectangle` Rectangle 1 3) `shouldBe` True

        it "if B is completely somewhere else" $ do
            (Rectangle 0 2 `containsRectangle` Rectangle 3 2) `shouldBe` True


{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell    #-}
module Test.Yage.Geometry.D2.Rectangle where

import Yage.Prelude
import Yage.Math

import Test.Hspec

import Yage.Geometry.D2.Rectangle

rectangleSpec :: Spec
rectangleSpec = do
    -- an rectangle opend top left from the origin to the origin
    let rect = Rectangle 0 (V2 2 2)


    describe "area on a Rectangle" $ do
        it "calculates the correct area" $ do
            area (Rectangle 0 (V2 3 5)) `shouldBe` 15

    
    describe "Rectangles fits" $ do
        it "if the extends of the first are smaller then the seconds extends, regardless of their positions in space" $ do 
            (Rectangle 0 1 `fits` Rectangle 0 2) `shouldBe` True

        it "not if the extends of first are bigger then the seconds extends, regardless of their centers" $ do
            (Rectangle 0 2 `fits` Rectangle 0 1) `shouldBe` False

        it "not if the areas are equal but have different alignments" $ do
            (Rectangle 0 (V2 2 3) `fits` Rectangle 0 (V2 3 2)) `shouldBe` False

        it "if the area and alignments are equal" $ do
            (Rectangle 0 (V2 2 3) `fits` Rectangle 0 (V2 2 3)) `shouldBe` True

    
    describe "A point (V2) is in a Rectangle" $ do
        it "if the point lies somewhere inside the Rectangle" $ do
            (rect `containsPoint` V2 1 1 ) `shouldBe` True

        it "if the point equals a corner of the Rectangle" $ do
            (rect `containsPoint` 0) `shouldBe` True

        it "if the point lies on border" $ do
            (rect `containsPoint` V2 0 1) `shouldBe` True


    describe "A point (V2) is NOT in a Rectangle" $ do
        it "if the point is outside on one axis" $ do
            (rect `containsPoint` V2 (-1) 1) `shouldBe` False

        it "if the point is outside on both axis" $ do
            (rect `containsPoint` V2 4 5) `shouldBe` False


    describe "Rectangle A contains Rectangle B completly" $ do
        let rectA = rect
        
        it "if their the xy1 corners are equal and B is smaller" $ do
            let rectB = Rectangle 0 (V2 1 1)
            (rectA `containsRectangle` rectB) `shouldBe` True
        
        it "if A and B are equal" $ do
            let rectB = rectA
            (rectA `containsRectangle` rectB) `shouldBe` True

        it "if A is bigger and B smaller and somewhere in A without touching any borders" $ do
            let rectB = Rectangle 1 1.5
            (rectA `containsRectangle` rectB) `shouldBe` True


    describe "Rectangle A contains Rectangle B NOT completly" $ do
        let rectA = rect
        
        it "if their xy1 corners are equal but B is bigger" $ do
            let rectB = Rectangle 0 3 
            (rectA `containsRectangle` rectB) `shouldBe` False
        
        it "if their xy2 corners are equal but B is bigger" $ do
            let rectB = Rectangle (-1) 2
            (rectA `containsRectangle` rectB) `shouldBe` False

        it "if B is completely somewhere else" $ do
            let rectB = Rectangle 5 6
            (rectA `containsRectangle` rectB) `shouldBe` False


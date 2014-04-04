{-# LANGUAGE TemplateHaskell    #-}
module Yage.Geometry.D2.Rectangle where

import Yage.Prelude
import Yage.Math
import Yage.Lens

import Data.Foldable (or, and)

data Rectangle a = Rectangle
    { _center :: !(V2 a) 
    , _extend :: !(V2 a)
    }
    deriving ( Eq, Show, Functor, Foldable, Traversable )

makeLenses ''Rectangle

width, height :: Lens' (Rectangle a) a
width  = extend._x
height = extend._y

area :: (Num a) => Rectangle a -> a
area rect = rect^.extend._x * rect^.extend._y

--newtype AreaComparedRectangle a = AreaComparedRectangle (Rectangle a) 
--instance (Eq a) => Eq (AreaComparedRectangle a) where

--instance (Ord a) => Ord (AreaComparedRectangle a) where
--    compare (AreaComparedRectangle r1) (AreaComparedRectangle r2) = areaOrdering r1 r2


fits :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
fits toFitRec intoRec = 
    toFitRec^.width  <= intoRec^.width &&
    toFitRec^.height <= intoRec^.height


extendOrdering :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Ordering
extendOrdering a b = compare (a^.extend) (b^.extend)


areaOrdering :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Ordering
areaOrdering a b = compare (area a) (area b)


intersect :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
intersect a b =
    let centerDiff = abs <$> a^.center - b^.center
        compExtend = a^.extend + b^.extend
    in not.or $ liftI2 (>) centerDiff compExtend


containsPoint :: ( Num a, Fractional a, Ord a ) => Rectangle a -> V2 a -> Bool
containsPoint rect pt =
    let centerDiff = abs <$> rect^.center - pt
    in not.or $ liftI2 (>) centerDiff (rect^.extend ^/ 2)


containsRectangle :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
containsRectangle outerRect innerRect = 
    let centerDiff = abs <$> innerRect^.center - outerRect^.center
        compExtend = innerRect^.extend + outerRect^.extend
    in and $ liftI2 (<=) centerDiff compExtend



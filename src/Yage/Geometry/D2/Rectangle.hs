{-# LANGUAGE TemplateHaskell    #-}
module Yage.Geometry.D2.Rectangle where

import Yage.Prelude
import Yage.Math
import Yage.Lens

data Rectangle a = Rectangle
    { _topLeft     :: !(V2 a)   -- ^ anchor for resizing
    , _bottomRight :: !(V2 a)
    }
    deriving ( Eq, Show, Functor, Foldable, Traversable )

makeLenses ''Rectangle

-----------------------------------------------------------------------------------------------------

width :: Num a => Lens' (Rectangle a) a
width  = lens getter setter
    where
    getter rect     = rect^.extend._x
    setter rect w   = rect & extend._x .~ w


height :: Num a => Lens' (Rectangle a) a
height = lens getter setter
    where
    getter rect     = rect^.extend._y
    setter rect h   = rect & extend._y .~ h



extend :: Num a => Lens' (Rectangle a) (V2 a)
extend = lens getter setter
    where
    getter rect     = rect^.bottomRight - rect^.topLeft
    setter rect e   = rect & bottomRight .~ rect^.topLeft + e



center :: Fractional a => Lens' (Rectangle a) (V2 a)
center = lens getter setter
    where
    getter rect     = rect^.topLeft + rect^.extend ^/ 2
    setter rect c   = let trans = rect^.center - c in translate rect trans



translate :: Num a => Rectangle a -> V2 a -> Rectangle a
translate r trans = r & topLeft     +~ trans
                      & bottomRight +~ trans



resize :: Num a => Rectangle a -> V2 a -> Rectangle a
resize r sz = r & extend *~ sz



area :: Num a => Rectangle a -> a
area rect = rect^.extend._x * rect^.extend._y



fits :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
fits toFitRec intoRec = 
    toFitRec^.width  <= intoRec^.width &&
    toFitRec^.height <= intoRec^.height



compareCenter :: (Ord a, Fractional a) => Rectangle a -> Rectangle a -> Ordering
compareCenter a b = (a^.center) `compare` (b^.center)



compareExtend :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Ordering
compareExtend a b = (a^.extend) `compare` (b^.extend)



compareArea :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Ordering
compareArea a b = (area a) `compare` (area b)



intersect :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
intersect a b = not $
    a^.topLeft._x > b^.bottomRight._x ||
    b^.topLeft._x > a^.bottomRight._x ||
    
    a^.topLeft._y > b^.bottomRight._y ||
    b^.topLeft._y > a^.bottomRight._y



containsPoint :: ( Num a, Ord a ) => Rectangle a -> V2 a -> Bool
containsPoint rect pt = not $ 
    rect^.topLeft._x       > pt^._x   ||
    rect^.topLeft._y       < pt^._y   ||
    
    rect^.bottomRight._x   < pt^._x   ||
    rect^.bottomRight._y   > pt^._y



containsRectangle :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
containsRectangle outerRect innerRect = not $
    outerRect^.topLeft._x     > innerRect^.topLeft._x       ||
    outerRect^.topLeft._y     < outerRect^.topLeft._y       ||

    outerRect^.bottomRight._y > innerRect^.bottomRight._y   ||
    outerRect^.bottomRight._x < innerRect^.bottomRight._x



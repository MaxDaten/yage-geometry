{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE FlexibleInstances        #-}
module Yage.Geometry.D2.Rectangle where

import Yage.Prelude
import Yage.Math
import Yage.Lens

import Data.Data

-- | Rectangle in right handed cartesian coordinates, x horizontal, y vertical
-- 0/0 is bottom left
data Rectangle a = Rectangle
    { _bottomLeft   :: !(V2 a)  
    -- ^ anchor for resizing
    , _topRight     :: !(V2 a)
    }
    deriving ( Eq, Show, Functor, Foldable, Traversable
             , Data, Typeable, Generic )

makeClassy ''Rectangle

class GetRectangle t a | t -> a where
    asRectangle :: Getter t (Rectangle a)

instance GetRectangle (Rectangle a) a where
    asRectangle = id


-----------------------------------------------------------------------------------------------------

width :: Num a => Lens' (Rectangle a) a
width  = lens getter setter
    where
    getter rect     = rect^.extend._x
    setter rect w   = rect & extend._x .~ w


height :: Num a => Lens' (Rectangle a) a
height = lens getter setter where
    getter rect     = rect^.extend._y
    setter rect h   = rect & extend._y .~ h


topLeft :: Lens' (Rectangle a) (V2 a)
topLeft = lens getter setter where
    getter rect     = V2 (rect^.bottomLeft._x) (rect^.topRight._y)
    setter rect tl  = rect & bottomLeft._x .~ tl^._x
                           & topRight._y   .~ tl^._y

bottomRight :: Lens' (Rectangle a) (V2 a)
bottomRight = lens getter setter where
    getter rect    = V2 (rect^.topRight._x) (rect^.bottomLeft._y) 
    setter rect br = rect & bottomLeft._y .~ br^._y
                          & topRight._x   .~ br^._x   


extend :: Num a => Lens' (Rectangle a) (V2 a)
extend = lens getter setter where
    getter rect     = rect^.topRight - rect^.bottomLeft
    setter rect e   = rect & topRight .~ rect^.bottomLeft + e



center :: Fractional a => Lens' (Rectangle a) (V2 a)
center = lens getter setter where
    getter rect     = rect^.bottomLeft + rect^.extend ^/ 2
    setter rect c   = let trans = rect^.center - c 
                      in translate rect trans



translate :: Num a => Rectangle a -> V2 a -> Rectangle a
translate r trans = r & bottomLeft     +~ trans
                      & topRight       +~ trans



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



intersects :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
intersects a b = not $
    a^.bottomLeft._x > b^.topRight._x ||
    b^.bottomLeft._x > a^.topRight._x ||
    
    a^.bottomLeft._y > b^.topRight._y ||
    b^.bottomLeft._y > a^.topRight._y



containsPoint :: ( Num a, Ord a ) => Rectangle a -> V2 a -> Bool
containsPoint rect pt = not $ 
    rect^.bottomLeft._x  > pt^._x   ||
    rect^.bottomLeft._y  > pt^._y   ||
    
    rect^.topRight._x    < pt^._x   ||
    rect^.topRight._y    < pt^._y



containsRectangle :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
containsRectangle outerRect innerRect = not $
    outerRect^.bottomLeft._x  > innerRect^.bottomLeft._x    ||
    outerRect^.bottomLeft._y  > innerRect^.bottomLeft._y    ||

    outerRect^.topRight._y    < innerRect^.topRight._y      ||
    outerRect^.topRight._x    < innerRect^.topRight._x



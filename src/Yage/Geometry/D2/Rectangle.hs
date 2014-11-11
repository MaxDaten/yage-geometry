{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE FlexibleInstances        #-}
module Yage.Geometry.D2.Rectangle where

import Yage.Prelude
import Yage.Math
import Yage.Lens

import Data.Data
import Data.Aeson.TH

-- | Rectangle in right handed cartesian coordinates
--
-- x horizontal, y vertical: no additional assumptions about
-- the orientation of the coord-system are made (e.g. flipped y-axis)
-- xy1 < xy2
data Rectangle a = Rectangle
    { _xy1   :: !(V2 a)
    -- ^ anchor for resizing
    , _xy2   :: !(V2 a)
    }
    deriving ( Eq, Show, Functor, Foldable, Traversable
             , Data, Typeable, Generic )

makeClassy ''Rectangle
$(deriveJSON defaultOptions ''Rectangle)

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


extend :: Num a => Lens' (Rectangle a) (V2 a)
extend = lens getter setter where
    getter rect     = rect^.xy2 - rect^.xy1
    setter rect e   = rect & xy2 .~ rect^.xy1 + e



center :: Fractional a => Lens' (Rectangle a) (V2 a)
center = lens getter setter where
    getter rect     = rect^.xy1 + rect^.extend ^/ 2
    setter rect c   = let trans = rect^.center - c
                      in translate rect trans



translate :: Num a => Rectangle a -> V2 a -> Rectangle a
translate r trans = r & xy1     +~ trans
                      & xy2     +~ trans


-- | resize with xy1 (top/left) as anchor
resize :: Num a => Rectangle a -> V2 a -> Rectangle a
resize r sz = r & extend *~ sz


-- | rescale both coordinates.
-- this is for e.g. useful if you want to transform from
-- a normalized to a destinct space
rescale :: Num a => Rectangle a -> V2 a -> Rectangle a
rescale r sz = r & xy1 *~ sz
                 & xy2 *~ sz


area :: Num a => Getter (Rectangle a) a
area = to calc where
    calc rect = rect^.extend._x * rect^.extend._y



fits :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
fits toFitRec intoRec =
    toFitRec^.width  <= intoRec^.width &&
    toFitRec^.height <= intoRec^.height



intersects :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
intersects a b = not $
    a^.xy1._x > b^.xy2._x ||
    b^.xy1._x > a^.xy2._x ||

    a^.xy1._y > b^.xy2._y ||
    b^.xy1._y > a^.xy2._y



containsPoint :: ( Num a, Ord a ) => Rectangle a -> V2 a -> Bool
containsPoint rect pt = not $
    rect^.xy1._x  > pt^._x   ||
    rect^.xy1._y  > pt^._y   ||

    rect^.xy2._x    < pt^._x   ||
    rect^.xy2._y    < pt^._y



containsRectangle :: ( Num a, Ord a ) => Rectangle a -> Rectangle a -> Bool
containsRectangle outerRect innerRect = not $
    outerRect^.xy1._x  > innerRect^.xy1._x    ||
    outerRect^.xy1._y  > innerRect^.xy1._y    ||

    outerRect^.xy2._y  < innerRect^.xy2._y      ||
    outerRect^.xy2._x  < innerRect^.xy2._x



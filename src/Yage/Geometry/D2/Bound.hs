{-# LANGUAGE TemplateHaskell    #-}
module Yage.Geometry.D2.Bound where

import Yage.Prelude hiding (or)
import Yage.Math
import Yage.Lens

import Data.Foldable (or)

data Bound a = Bound
    { _center :: !(V2 a) 
    , _extend :: !(V2 a)
    }
    deriving ( Eq, Show, Functor, Foldable, Traversable )

makeLenses ''Bound

width, height :: Lens' (Bound a) a
width  = extend._x
height = extend._y


topLeft :: ( Num a, Fractional a ) => Lens' (Bound a) (V2 a)
topLeft = lens getter setter
    where
    getter rect     = rect^.center - V2 1 (-1) * rect^.extend ^/ 2
    setter rect tl  = fromCorners tl (rect^.bottomRight)


bottomRight :: ( Num a, Fractional a ) => Lens' (Bound a) (V2 a)
bottomRight = lens getter setter
    where
    getter rect     = rect^.center + V2 1 (-1) * rect^.extend ^/ 2
    setter rect br  = fromCorners (rect^.topLeft) br


corners :: ( Num a, Fractional a ) => Lens' (Bound a) (V2 a, V2 a)
corners = lens getter setter
    where
    getter rect         = (rect^.topLeft, rect^.bottomRight)
    setter _ (tl, br)   = fromCorners tl br


unitAtZero :: Num a => Bound a
unitAtZero = Bound 0 1


fromCorners :: ( Num a, Fractional a ) 
            => V2 a                     -- ^ top left
            -> V2 a                     -- ^ bottom right
            -> Bound a
fromCorners tl br =
    let e = br - tl
        c = tl + V2 1 (-1) * e ^/ 2
    in Bound c e



translate :: Num a => Bound a -> V2 a -> Bound a
translate r trans = r & center +~ trans


resize :: Num a => Bound a -> V2 a -> Bound a
resize r sz = r & extend *~ sz


area :: Num a => Bound a -> a
area rect = rect^.extend._x * rect^.extend._y


fits :: ( Num a, Ord a ) => Bound a -> Bound a -> Bool
fits toFitRec intoRec = 
    toFitRec^.width  <= intoRec^.width &&
    toFitRec^.height <= intoRec^.height


compareCenter :: ( Num a, Ord a ) => Bound a -> Bound a -> Ordering
compareCenter a b = (a^.center) `compare` (b^.center)


compareExtend :: ( Num a, Ord a ) => Bound a -> Bound a -> Ordering
compareExtend a b = (a^.extend) `compare` (b^.extend)


compareArea :: ( Num a, Ord a ) => Bound a -> Bound a -> Ordering
compareArea a b = (area a) `compare` (area b)


intersect :: ( Num a, Ord a ) => Bound a -> Bound a -> Bool
intersect a b =
    let centerDiff = abs <$> a^.center - b^.center
        compExtend = a^.extend + b^.extend
    in not.or $ liftI2 (>) centerDiff compExtend


containsPoint :: ( Num a, Fractional a, Ord a ) => Bound a -> V2 a -> Bool
containsPoint rect pt =
    let centerDiff = abs <$> rect^.center - pt
    in not.or $ liftI2 (>) centerDiff (rect^.extend ^/ 2)


containsBound :: ( Num a, Ord a, Fractional a ) => Bound a -> Bound a -> Bool
containsBound outerRect innerRect = not $
    outerRect^.topLeft._x     > innerRect^.topLeft._x       ||
    outerRect^.topLeft._y     < outerRect^.topLeft._y       ||

    outerRect^.bottomRight._y > innerRect^.bottomRight._y   ||
    outerRect^.bottomRight._x < innerRect^.bottomRight._x



{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}
module Yage.Geometry.D3.Grid where

import Yage.Prelude hiding (Index)
import Yage.Lens

import Yage.Data.List (init, tail, chunksOf)

import Yage.Math

import Yage.Geometry.Elements

data Grid v = Grid
  { _gridSections :: [v] }
  deriving ( Show, Functor, Foldable, Traversable, Generic )

makeLenses ''Grid


gridPos :: (Floating a, Enum a) => V2 Int -> V2 a -> Grid (Face (V3 a))
gridPos divs dim =
    let V2 left back     = dim / (-2.0)
        V2 xStep zStep   = 1.0 / (fromIntegral <$> divs)
        f x z            = V3 (left + fromIntegral x * xStep) 0.0 (back + fromIntegral z * zStep)
    in buildGrid f divs


gridUV :: (Floating a, Enum a) => V2 Int -> Grid (Face (V2 a))
gridUV divs =
  let V2 xStep zStep   = 1.0 / (fromIntegral <$> divs)
      f x z            = V2 (fromIntegral x * xStep) (fromIntegral z * zStep)
  in buildGrid f divs

-- | creates a grid along the xz plane with its center in origin and 1/1 dimension
-- divisions along x and y. for a propper positioning of the center in origin, divisions
-- sould be even
-- TODO offset left
buildGrid :: (Int -> Int -> v) -> V2 Int -> Grid (Face v)
buildGrid f (V2 xdiv zdiv)
  | xdiv < 1 || zdiv < 1 = error "invalid divisions"
  | otherwise = 
    let verts            = [ f x z | z <- [ 0 .. zdiv ], x <- [ 0 .. xdiv ] ]
        rows             = chunksOf (xdiv+1) verts
        faces            = concat $ [ rowFaces r n | r <- init rows | n <- tail rows ]
    in Grid faces
  where
    rowFaces row nextrow = [ Face a b c d | a <- init row | b <- init nextrow | c <- tail nextrow | d <- tail row ]



instance Applicative Grid where
  pure = Grid . singleton
  (Grid fs) <*> (Grid as) = Grid (zipWith ($) fs as)


instance (Floating a, Enum a) => Default (Grid (Face (V3 a))) where
  def = gridPos 1 1

instance HasSurfaces Grid where
  surfaces = singleton . Surface . _gridSections

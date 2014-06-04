{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
module Yage.Geometry.D3.Cube where

import Yage.Prelude hiding (Index, toList)
import Yage.Lens

import Data.Foldable       (toList)

import Linear (V2(..), V3(..))
import Yage.Geometry.Elements

---------------------------------------------------------------------------------------------------
-- Primitives
data Cube v = Cube
  { _cubeRight     :: v -- GL_TEXTURE_CUBE_MAP_POSITIVE_X​
  , _cubeLeft      :: v -- GL_TEXTURE_CUBE_MAP_NEGATIVE_X​
  , _cubeTop       :: v -- GL_TEXTURE_CUBE_MAP_POSITIVE_Y​
  , _cubeBottom    :: v -- GL_TEXTURE_CUBE_MAP_NEGATIVE_Y​
  , _cubeFront     :: v -- GL_TEXTURE_CUBE_MAP_POSITIVE_Z​
  , _cubeBack      :: v -- GL_TEXTURE_CUBE_MAP_NEGATIVE_Z​
  } deriving ( Show, Functor, Foldable, Traversable, Generic )

makeLenses ''Cube

type CubePos a = Cube (Face (V3 a))
type CubeUV a  = Cube (Face (V2 a))

cubePos :: Fractional a => V3 a -> CubePos a
cubePos dim = 
  let V3 x y z = dim / 2.0
  in Cube
    { _cubeRight     = Face ( V3   x    y    z  )
                            ( V3   x  (-y)   z  )
                            ( V3   x  (-y) (-z) )
                            ( V3   x    y  (-z) )
    
    , _cubeLeft      = Face ( V3 (-x)   y  (-z) )
                            ( V3 (-x) (-y) (-z) )
                            ( V3 (-x) (-y)   z  )
                            ( V3 (-x)   y    z  )
    
    , _cubeTop       = Face ( V3 (-x)   y  (-z) )
                            ( V3 (-x)   y    z  )
                            ( V3   x    y    z  )
                            ( V3   x    y  (-z) )
    
    , _cubeBottom    = Face ( V3 (-x) (-y)   z  )
                            ( V3 (-x) (-y) (-z) )
                            ( V3   x  (-y) (-z) )
                            ( V3   x  (-y)   z  )
    
    , _cubeFront     = Face ( V3 (-x)   y    z  )
                            ( V3 (-x) (-y)   z  )
                            ( V3   x  (-y)   z  )
                            ( V3   x    y    z  )
    
    , _cubeBack      = Face ( V3 (-x) (-y) (-z) )
                            ( V3 (-x)   y  (-z) )
                            ( V3   x    y  (-z) )
                            ( V3   x  (-y) (-z) )
    }


-- | creates one UV set for all faces
-- only matches to the cubePos order of positions
cubeSingleUV :: Fractional a => CubeUV a
cubeSingleUV =
  let uvFace = Face (V2 0 1) (V2 0 0) (V2 1 0) (V2 1 1)
  in Cube uvFace uvFace uvFace uvFace uvFace uvFace


instance Applicative Cube where
  pure a = Cube a a a a a a
  (Cube fa fb fc fd fe fg) <*> (Cube a b c d e g) 
    = Cube (fa a) (fb b) (fc c) (fd d) (fe e) (fg g)


instance Fractional a => Default (CubePos a) where
  def = cubePos 1

instance Fractional a => Default (CubeUV a) where
  def = cubeSingleUV

instance HasSurfaces Cube where
  surfaces = fmap (Surface . singleton) . toList


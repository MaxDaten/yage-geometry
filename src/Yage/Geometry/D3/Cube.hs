{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
module Yage.Geometry.D3.Cube where

import Yage.Prelude hiding (Index, (<$$>), toList)
import Yage.Lens

import Data.Foldable       (toList)
import Control.Applicative hiding ((<**>))

import Linear (V2(..), V3(..))
--import Yage.Geometry.D3.Basic
import Yage.Geometry.Vertex
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


type CubeUV pn tn a = Cube (Face (Vertex (P3T2 pn tn a)))

cubePos :: Fractional a => V3 a -> Cube (Face (V3 a))
cubePos dim = 
  let V3 x y z = dim / 2.0
  in Cube
    { _cubeRight     = Face ( V3   x    y    z  )
                            ( V3   x  (-y)   z  )
                            ( V3   x  (-y) (-z) )
                            ( V3    x   y  (-z) )
    
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
    
    , _cubeBack      = Face ( V3   x    y  (-z) )
                            ( V3   x  (-y) (-z) )
                            ( V3 (-x) (-y) (-z) )
                            ( V3 (-x)   y  (-z) )
    }

makeLenses ''Cube

-- | creates one UV set for all faces
-- only matches to the cubePos order of positions
cubeSingleUV :: Fractional a => Cube (Face (V2 a))
cubeSingleUV =
  let uvFace = Face (V2 0 1) (V2 0 0) (V2 1 0) (V2 1 1)
  in Cube uvFace uvFace uvFace uvFace uvFace uvFace


cubeWithUV :: (Fractional a) => Cube (Face (V2 a)) -> V3 a -> CubeUV pn tn a
cubeWithUV cubeUV dim =
  let cube = cubePos dim
  in merge <$$> cube <**> cubeUV
  where
  merge :: (Fractional a, v ~ Vertex (P3T2 pn tn a)) => V3 a -> V2 a -> v
  merge p t = position3 =: p <+> 
              texture2  =: t
  
  (<**>) = liftA2 (<*>)
  f <$$> x = liftA2 (<$>) (pure f) x


defaultCube :: Fractional a => V3 a -> CubeUV pn tn a
defaultCube = cubeWithUV cubeSingleUV


instance Applicative Cube where
  pure a = Cube a a a a a a
  (Cube fa fb fc fd fe fg) <*> (Cube a b c d e g) 
    = Cube (fa a) (fb b) (fc c) (fd d) (fe e) (fg g)


instance Fractional a => Default (CubeUV pn tn a) where
  def = defaultCube 1

instance HasSurfaces Cube where
  surfaces cube = Surface . singleton <$> toList cube


{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Yage.Geometry.Vertex
    ( module Yage.Geometry.Vertex
    , module Data.Vinyl
    ) where

-- import Yage.Prelude
import Data.Vinyl
import Linear


type Position3 name a = name ::: V3 a
type Position2 name a = name ::: V2 a
type Normal3   name a = name ::: V3 a
type Texture2  name a = name ::: V2 a
type Color4    name a = name ::: V4 a
type Vertex rs = PlainRec rs

type P3 pn a      = '[Position3 pn a]
type P3N3 pn nn a = [Position3 pn a, Normal3 nn a]

position3 :: Position3 name a
position3 = Field

position2 :: Position2 name a
position2 = Field

normal3   :: Normal3 name a
normal3   = Field

texture2  :: Texture2 name a
texture2  = Field

color4    :: Color4 name a
color4    = Field


--data VertexData pn nn tn a =
--      Position3 (pn ::: V3 a)
--    | Normal3   (nn ::: V3 a)
--    | Texture2  (tn ::: V3 a)
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
type Tangent3  name a = name ::: V3 a
type Tangent4  name a = name ::: V4 a
type Texture2  name a = name ::: V2 a
type Color4    name a = name ::: V4 a
type Vertex rs = PlainRec rs

--{--
type P3 pn a                = '[Position3 pn a]
type T2 tn a                = '[Texture2 tn a]

type P3N3 pn nn a           = [ Position3 pn a
                              , Normal3 nn a
                              ]

type P3T2 pn tn a           = [ Position3 pn a
                              , Texture2 tn a
                              ]

type P3T2N3  pn tx nn a     = [ Position3 pn a
                              , Texture2 tx a
                              , Normal3 nn a
                              ]

type P3T2NT3 pn tx nn tg a  = [ Position3 pn a
                              , Texture2 tx a
                              , Normal3 nn a
                              , Tangent3 tg a
                              ]
--}

position3 :: Position3 name a
position3 = Field

position2 :: Position2 name a
position2 = Field

normal3   :: Normal3 name a
normal3   = Field

tangent3  :: Tangent3 name a
tangent3  = Field

tangent4 :: Tangent4 name a
tangent4 = Field

texture2  :: Texture2 name a
texture2  = Field

color4    :: Color4 name a
color4    = Field

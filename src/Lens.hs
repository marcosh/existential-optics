{-# LANGUAGE FlexibleContexts #-}

module Lens where

import Morph
import Optics

-- base
import Data.Tuple (swap)

-- DATA TYPE

type Lens = Optic (,)

type Lens' s a = Optic' (,) s a

-- COMMON LENSES

_1 :: Morph (,) f => Optic f (a, c) (b, c) a b
_1 = morph _1Lens
  where
    _1Lens :: Lens (a, c) (b, c) a b
    _1Lens = Optic swap swap

_2 :: Morph (,) f => Optic f (c, a) (c, b) a b
_2 = morph _2Lens
  where
    _2Lens :: Lens (c, a) (c, b) a b
    _2Lens = Optic id id

-- COMMON OPERATIONS

view :: Morph f (,) => Optic f s t a b -> s -> a
view = viewLens . morph
  where
    viewLens :: Lens s t a b -> s -> a
    viewLens (Optic f _) = snd . f

over :: Morph f (,) => Optic f s t a b -> (a -> b) -> s -> t
over = overLens . morph
  where
    overLens :: Lens s t a b -> (a -> b) -> s -> t
    overLens (Optic f g) h = g . fmap h . f

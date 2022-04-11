{-# LANGUAGE FlexibleContexts #-}

module Prism where

import Morph
import Optics

-- DATA TYPE

type Prism = Optic Either

type Prism' s a = Prism s s a a

-- COMMON PRISMS

swapEither :: Either a b -> Either b a
swapEither (Left a ) = Right a
swapEither (Right b) = Left b

_Left :: Morph Either f => Optic f (Either a c) (Either b c) a b
_Left = morph _LeftPrism
  where
    _LeftPrism :: Prism (Either a c) (Either b c) a b
    _LeftPrism = Optic swapEither swapEither

_Right :: Morph Either f => Optic f (Either c a) (Either c b) a b
_Right = morph _RightPrism
  where
    _RightPrism :: Prism (Either c a) (Either c b) a b
    _RightPrism = Optic id id

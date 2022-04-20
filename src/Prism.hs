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

-- COMMON OPERATIONS

preview :: Morph f Either => Optic f s t a b -> s -> Maybe a
preview = previewPrism . morph
  where
    previewPrism :: Prism s t a b -> s -> Maybe a
    previewPrism (Optic f _) = rightToMaybe . f

    rightToMaybe :: Either a b -> Maybe b
    rightToMaybe (Left  _) = Nothing
    rightToMaybe (Right b) = Just b

review :: Morph f Either => Optic f s t a b -> b -> t
review = reviewPrism . morph
  where
    reviewPrism :: Prism s t a b -> b -> t
    reviewPrism (Optic _ g) = g . Right

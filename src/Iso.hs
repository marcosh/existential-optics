{-# LANGUAGE FlexibleContexts #-}

module Iso where

import ConstOp
import Morph
import Optics

-- base
import Data.Tuple (swap)

-- DATA TYPE

type Iso = Optic ConstOp

type Iso' s a = Optic' ConstOp s a

-- COMMON ISOS

equality :: Morph ConstOp f => Optic' f a a
equality = morph equalityIso
  where
    equalityIso :: Iso' a a
    equalityIso = Optic ConstOp getConstOp

swapped :: Morph ConstOp f => Optic f (a, b) (c, d) (b, a) (d, c)
swapped = morph swappedIso
  where
    swappedIso :: Iso (a, b) (c, d) (b, a) (d, c)
    swappedIso = Optic (ConstOp . swap) (swap. getConstOp)

flipped :: Morph ConstOp f => Optic f (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = morph flippedIso
  where
    flippedIso :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
    flippedIso = Optic (ConstOp . flip) (flip . getConstOp)

curried :: Morph ConstOp f => Optic f ((a, b) -> c) ((d, e) -> g) (a -> b -> c) (d -> e -> g)
curried = morph curriedIso
  where
    curriedIso :: Iso ((a, b) -> c) ((d, e) -> g) (a -> b -> c) (d -> e -> g)
    curriedIso = Optic (ConstOp . curry) (uncurry . getConstOp)

uncurried :: Morph ConstOp f => Optic f (a -> b -> c) (d -> e -> g) ((a, b) -> c) ((d, e) -> g)
uncurried = morph uncurriedIso
  where
    uncurriedIso :: Iso (a -> b -> c) (d -> e -> g) ((a, b) -> c) ((d, e) -> g)
    uncurriedIso = Optic (ConstOp . uncurry) (curry . getConstOp)

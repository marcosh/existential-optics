{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Optics where

import Affine
import Associativity
import ConstOp
import Morph
import PowerSeries
import TypeTuples

-- base
import Data.Bifunctor
import Data.Tuple (swap)

-- constraints
import Data.Constraint (Dict)

-- GENERAL OPTICS

data Optic f s t a b = forall c. Optic (s -> f c a) (f c b -> t)

morph :: Morph f g => Optic f s t a b -> Optic g s t a b
morph (Optic f g) = Optic (f2g . f) (g . g2f)

compose :: (ExistentiallyAssociative f, forall x. Functor (f x)) => Optic f s t u v -> Optic f u v a b -> Optic f s t a b
compose (Optic su vt) (Optic ua bv) = Optic (existentialAssociateL . fmap ua . su) (vt . fmap bv . existentialAssociateR)

(%) :: (ExistentiallyAssociative h, forall x. Functor (h x), Morph f h, Morph g h) => Optic f s t u v -> Optic g u v a b -> Optic h s t a b
(%) opticF opticG = compose (morph opticF) (morph opticG)

-- ISOS

type Iso = Optic ConstOp

type Iso' s a = Iso s s a a

swapped :: Iso' (a, b) (b, a)
swapped = Optic (ConstOp . swap) (swap. getConstOp)

-- LENSES

type Lens = Optic (,)

type Lens' s a = Lens s s a a

_1 :: Lens (a, c) (b, c) a b
_1 = Optic swap swap

_2 :: Lens (c, a) (c, b) a b
_2 = Optic id id

-- PRISMS

type Prism = Optic Either

type Prism' s a = Prism s s a a

swapEither :: Either a b -> Either b a
swapEither (Left a ) = Right a
swapEither (Right b) = Left b

_Left :: Prism (Either a c) (Either b c) a b
_Left = Optic swapEither swapEither

_Right :: Prism (Either c a) (Either c b) a b
_Right = Optic id id

-- GRATES

type Grate = Optic (->)

-- AFFINE TRAVERSALS

type AffineAssociated a b = '(Affine a (Fst b), (Snd a, Snd b))

instance ExistentiallyAssociative Affine where
  type E Affine a b = AffineAssociated a b

  existentialAssociateL :: Affine a (Affine b c) -> Affine (AffineAssociated a b) c
  existentialAssociateL (Affine (Left a0))                            = Affine (Left (Affine (Left a0)))
  existentialAssociateL (Affine (Right (a1, Affine (Left b0))))       = Affine (Left (Affine (Right (a1, b0))))
  existentialAssociateL (Affine (Right (a1, Affine (Right (b1, c))))) = Affine (Right ((a1, b1), c))

  existentialAssociateR :: Affine (AffineAssociated a b) c -> Affine a (Affine b c)
  existentialAssociateR (Affine (Left (Affine (Left a0))))        = Affine (Left a0)
  existentialAssociateR (Affine (Left (Affine (Right (a1, b0))))) = Affine (Right (a1, Affine (Left b0)))
  existentialAssociateR (Affine (Right ((a1, b1), c)))            = Affine (Right (a1, Affine (Right (b1, c))))

type AffineTraversal = Optic Affine

-- TRAVERSALS

-- instance ExistentiallyAssociative PowerSeries where
--   type E PowerSeries a b = PowerSeries a b
--   type C PowerSeries a b c = Semigroup c

--   existentialAssociateL :: Semigroup c => PowerSeries a (PowerSeries b c) -> PowerSeries (PowerSeries a b) c
--   existentialAssociateL (Const a)           = Const (Const a)
--   existentialAssociateL (Poly psapsbc psbc) = actionProduct (\ps b -> _wB) (existentialAssociateL psapsbc) psbc

--   existentialAssociateR :: PowerSeries (PowerSeries a b) c -> PowerSeries a (PowerSeries b c)
--   existentialAssociateR ps = _

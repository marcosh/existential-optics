{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- GENERAL OPTICS

data Optic f s t a b = forall c. Optic (s -> f c a) (f c b -> t)

type Optic' f s a = Optic f s s a a

-- CHANGE OPTIC TYPE

morph :: Morph f g => Optic f s t a b -> Optic g s t a b
morph (Optic f g) = Optic (f2g . f) (g . g2f)

-- COMPOSE OPTICS

(%) :: (ExistentiallyAssociative f, forall x. Functor (f x)) => Optic f s t u v -> Optic f u v a b -> Optic f s t a b
(%) (Optic su vt) (Optic ua bv) = Optic (existentialAssociateL . fmap ua . su) (vt . fmap bv . existentialAssociateR)

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

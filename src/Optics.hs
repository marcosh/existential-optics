{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Optics where

import Associativity

-- base
import Control.Applicative (Const(..))
import Data.Tuple (swap)

-- GENERAL OPTICS

data Optic f s t a b = forall c. Optic (s -> f c a) (f c b -> t)

compose :: (ExistentiallyAssociative f, forall x. Functor (f x)) => Optic f s t u v -> Optic f u v a b -> Optic f s t a b
compose (Optic su vt) (Optic ua bv) = Optic (existentialAssociateL . fmap ua . su) (vt . fmap bv . existentialAssociateR)

-- ISOS

type Iso s a = Optic Const s s a a

swapped :: Iso (a, b) (b, a)
swapped = Optic Const getConst

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

type family Fst (a :: (k1, k2)) where
  Fst '(x, y) = x

type family Snd (a :: (k1, k2)) where
  Snd '(x, y) = y

newtype Affine a b = Affine (Either (Fst a) (Snd a, b))
  deriving Functor

-- possible alternative definition
--
-- data Affine a b where
--   Affine :: a ~ (a0, a1) => Either a0 (a1, b) -> Affine a b

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

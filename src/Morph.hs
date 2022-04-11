{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Morph where

import Affine
import Associativity
import ConstOp

-- base
import Control.Arrow ((&&&))
import Data.Void ( absurd, Void )

class Morph (f :: k1 -> * -> *) (g :: k2 -> * -> *) where
  type M f g (c :: k1) :: k2
  f2g :: f c a -> g (M f g c) a
  g2f :: g (M f g c) a -> f c a

instance Morph f f where
  type M f f c = c

  f2g :: f c a -> f c a
  f2g = id

  g2f :: f c a -> f c a
  g2f = id

-- ConstOp

instance Morph ConstOp (,) where
  type M ConstOp (,) c = ()

  f2g :: ConstOp c a -> ((), a)
  f2g (ConstOp a) = ((), a)

  g2f :: ((), a) -> ConstOp c a
  g2f ((), a) = ConstOp a

instance Morph ConstOp Either where
  type M ConstOp Either c = Void

  f2g :: ConstOp c a -> Either Void a
  f2g (ConstOp a) = Right a

  g2f :: Either Void a -> ConstOp c a
  g2f = either absurd ConstOp

instance Morph ConstOp (->) where
  type M ConstOp (->) c = ()

  f2g :: ConstOp c a -> (() -> a)
  f2g (ConstOp a) = const a

  g2f :: (() -> a) -> ConstOp c a
  g2f f = ConstOp (f ())

instance Morph ConstOp Affine where
  type M ConstOp Affine c = '(Void, ())

  f2g :: ConstOp c a -> Affine '(Void, ()) a
  f2g (ConstOp a) = Affine (Right ((), a))

  g2f :: Affine '(Void, ()) a -> ConstOp c a
  g2f (Affine e) = either absurd (ConstOp . snd) e

-- (,)

instance Morph (,) Affine where
  type M (,) Affine c = '(Void, c)

  f2g :: (c, a) -> Affine '(Void, c) a
  f2g (c, a) = Affine (Right (c, a))

  g2f :: Affine '(Void, c) a -> (c, a)
  g2f (Affine e) = either absurd id e

-- Either

instance Morph Either Affine where
  type M Either Affine c = '(c, ())

  f2g :: Either c a -> Affine '(c, ()) a
  f2g e = Affine (fmap ((),) e)

  g2f :: Affine '(c, ()) a -> Either c a
  g2f (Affine e) = fmap snd e

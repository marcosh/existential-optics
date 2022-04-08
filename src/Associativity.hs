{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Associativity where

import ConstOp

-- base
import Data.Kind (Constraint)

class ExistentiallyAssociative (f :: k -> * -> *) where
  type E f (a :: k) (b :: k) :: k

  existentialAssociateL :: f a (f b c) -> f (E f a b) c
  existentialAssociateR :: f (E f a b) c -> f a (f b c)

instance ExistentiallyAssociative ConstOp where
  type E ConstOp a b = ()

  existentialAssociateL :: ConstOp a (ConstOp b c) -> ConstOp () c
  existentialAssociateL (ConstOp (ConstOp c)) = ConstOp c

  existentialAssociateR :: ConstOp () c -> ConstOp a (ConstOp b c)
  existentialAssociateR (ConstOp c) = ConstOp (ConstOp c)

instance ExistentiallyAssociative (,) where
  type E (,) a b = (a, b)

  existentialAssociateL :: (a, (b, c)) -> ((a, b), c)
  existentialAssociateL (a, (b, c)) = ((a, b), c)

  existentialAssociateR :: ((a, b), c) -> (a, (b, c))
  existentialAssociateR ((a, b), c) = (a, (b, c))

instance ExistentiallyAssociative Either where
  type E Either a b = Either a b

  existentialAssociateL :: Either a (Either b c) -> Either (Either a b) c
  existentialAssociateL (Left a)          = Left (Left a)
  existentialAssociateL (Right (Left b))  = Left (Right b)
  existentialAssociateL (Right (Right c)) = Right c

  existentialAssociateR :: Either (Either a b) c -> Either a (Either b c)
  existentialAssociateR (Left (Left a))  = Left a
  existentialAssociateR (Left (Right b)) = Right (Left b)
  existentialAssociateR (Right c)        = Right (Right c)

instance ExistentiallyAssociative (->) where
  type E (->) a b = (a, b)

  existentialAssociateL :: (a -> (b -> c)) -> ((a, b) -> c)
  existentialAssociateL = uncurry

  existentialAssociateR :: ((a, b) -> c) -> (a -> (b -> c))
  existentialAssociateR = curry

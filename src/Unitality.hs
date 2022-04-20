{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Unitality where

import Affine
import ConstOp

-- base
import Data.Void

class ExistentiallyUnital (f :: k -> * -> *) where
  type U f :: k

  unitor    :: a -> f (U f) a
  unitorInv :: f (U f) a -> a

instance ExistentiallyUnital ConstOp where
  type U ConstOp = ()

  unitor :: a -> ConstOp () a
  unitor = ConstOp

  unitorInv :: ConstOp () a -> a
  unitorInv = getConstOp

instance ExistentiallyUnital (,) where
  type U (,) = ()

  unitor :: a -> ((), a)
  unitor = ((), )

  unitorInv :: ((), a) -> a
  unitorInv = snd

instance ExistentiallyUnital Either where
  type U Either = Void

  unitor :: a -> Either Void a
  unitor = Right

  unitorInv :: Either Void a -> a
  unitorInv = either absurd id

instance ExistentiallyUnital (->) where
  type U (->) = ()

  unitor :: a -> () -> a
  unitor = const

  unitorInv :: (() -> a) -> a
  unitorInv = ($ ())

instance ExistentiallyUnital Affine where
  type U Affine = '(Void, ())

  unitor :: a -> Affine '(Void, ()) a
  unitor = Affine . Right . ((), )

  unitorInv :: Affine '(Void, ()) a -> a
  unitorInv (Affine e) = either absurd snd e

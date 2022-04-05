{-# LANGUAGE KindSignatures #-}

module ConstOp where

newtype ConstOp (a :: *) b = ConstOp {getConstOp :: b}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Vect where

data Z
data S a

data Vect (n :: *) (a :: *) where
  VNil  :: Vect Z a
  (:::) :: a -> Vect n a -> Vect (S n) a
infixr 5 :::

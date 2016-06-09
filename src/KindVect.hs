{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module KindVect where

data Nat
  = S Nat
  | Z

type family (x :: Nat) :+ (y :: Nat) :: Nat where
  'Z   :+ y = y
  'S x :+ y = 'S (x :+ y)

data Vect (n :: Nat) (a :: *) where
  VNil  :: Vect 'Z a
  (:::) :: a -> Vect n a -> Vect ('S n) a
infixr 5 :::

infixr 5 +++
(+++) :: Vect m a -> Vect n a -> Vect (m :+ n) a
VNil       +++ ys = ys
(x ::: xs) +++ ys = x ::: xs +++ ys

vhead :: Vect ('S n) a -> a
vhead (x ::: _) = x

vtail :: Vect ('S n) a -> a
vtail (x ::: VNil)         = x
vtail (_ ::: xs@(_ ::: _)) = vtail xs

data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

vlookup :: Fin n -> Vect n a -> a
vlookup FZ     (x ::: _)  = x
vlookup (FS i) (_ ::: xs) = vlookup i xs

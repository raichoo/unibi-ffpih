{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module HList where

data HList (as :: [*]) where
  HNil  :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)
infixr 5 :::

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show (HList as), Show a) => Show (HList (a ': as)) where
  show (x ::: xs) = show x ++ " ::: " ++ show xs

singleton :: a -> HList '[a]
singleton x = x ::: HNil

type family (ts :: [k]) :++ (ts' :: [k]) :: [k] where
  '[]       :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)

infixr 5 +++
(+++) :: HList ts -> HList ts' -> HList (ts :++ ts')
HNil       +++ ys = ys
(x ::: xs) +++ ys = x ::: xs +++ ys

type family Reverse (ts :: [k]) :: [k] where
  Reverse '[]       = '[]
  Reverse (x ': xs) = Reverse xs :++ '[x]

hreverse :: HList ts -> HList (Reverse ts)
hreverse HNil       = HNil
hreverse (x ::: xs) = hreverse xs +++ singleton x

data Elem (ts :: [*]) (t :: *) where
  Stop :: Elem (x ': xs) x
  Pop  :: Elem ys x -> Elem (y ': ys) x

hlookup :: Elem ts x -> HList ts -> x
hlookup Stop    (x ::: _)  = x
hlookup (Pop i) (_ ::: xs) = hlookup i xs

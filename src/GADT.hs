{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module GADT where

import Data.Bool (bool)

data Expr a where
  Bool :: Bool -> Expr Bool
  Int  :: Int  -> Expr Int
  Add  :: Expr Int -> Expr Int -> Expr Int
  Eq   :: Eq b => Expr b -> Expr b -> Expr Bool
  If   :: Expr Bool -> Expr a -> Expr a -> Expr a

-- data Expr a
--   = a ~ Bool => Bool Bool
--   | a ~ Int => Int Int
--   | a ~ Int => Add (Expr Int) (Expr Int)
--   | forall b. (a ~ Bool, Eq b) => Eq (Expr b) (Expr b)
--   | If (Expr Bool) (Expr a) (Expr a)

eval :: Expr a -> a
eval (Bool b)   = b
eval (Int i)    = i
eval (Add x y)  = eval x + eval y
eval (Eq x y)   = eval x == eval y
eval (If c t f) = eval f `bool` eval t $ eval c

test :: Int -> Int -> Bool
test x y = eval $
  If (Eq (Int x) (Int y))
    (Bool True)
    (Bool False)

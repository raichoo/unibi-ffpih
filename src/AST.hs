module AST where

import Control.Monad.Except

import Text.Printf

data Expr
  = Bool Bool
  | Int Int
  | Add Expr Expr
  | Eq Expr Expr
  | If Expr Expr Expr
  deriving Show

data Ty
  = IntTy
  | BoolTy
  deriving (Eq, Show)

data TypeError
  = Expected Ty (Expr, Ty)

instance Show TypeError where
  show (Expected ty (term, termTy)) = printf
    "Expected: %s \n\
    \Found: `%s` of %s"
    (show ty)
    (show term)
    (show termTy)


checkTy :: Expr -> Except TypeError Ty
-- `Bool` chase
checkTy (Bool _) = return BoolTy

-- `Int` case
checkTy (Int  _) = return IntTy

-- `Add` case
checkTy (Add x y)  = do
  xTy <- checkTy x

  when (xTy /= IntTy) $
    throwError (Expected IntTy (x, xTy))

  yTy <- checkTy y

  when (yTy /= IntTy) $
    throwError (Expected IntTy (y, yTy))

  return IntTy

-- `Eq` case
checkTy (Eq x y) = do
  xTy <- checkTy x
  yTy <- checkTy y

  when (xTy /= yTy) $
    throwError (Expected xTy (y, yTy))

  return BoolTy

-- `If` case
checkTy (If c t f) = do
  cTy <- checkTy c

  when (cTy /= BoolTy) $
    throwError (Expected BoolTy (c, cTy))

  tTy <- checkTy t
  fTy <- checkTy f

  when (tTy /= fTy) $
    throwError (Expected tTy (f, fTy))

  return tTy

check :: Expr -> IO ()
check = either print print . runExcept . checkTy

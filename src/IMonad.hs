{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module IMonad where

import Prelude (Show)

newtype Rocket a = Rocket
  { unRocket :: a }
  deriving Show

class IndexedMonad (m :: k -> k -> * -> *)  where
  type Unit m :: k
  type Plus m (i :: k) (o :: k) :: k

  (>>=) :: m i o a -> (a -> m o o' b) -> m i (Plus m o o') b

  return :: a -> m (Unit m) (Unit m) a

data RocketState
  = NoFuel
  | NoGas
  | HasFuel
  | HasGas
  | Ready
  | Ignited
  | Launched

newtype LaunchSequence (i :: RocketState) (o :: RocketState) r
  = LaunchSequence { getRocket :: r }

instance IndexedMonad LaunchSequence where
  type Unit LaunchSequence = 'NoFuel

  type Plus LaunchSequence 'NoFuel 'HasFuel = 'NoGas
  type Plus LaunchSequence 'NoGas  'HasGas  = 'Ready
  type Plus LaunchSequence 'Ready  'Ignited = 'Launched
  type Plus LaunchSequence a       a        = a

  return = LaunchSequence

  LaunchSequence r >>= f =
    LaunchSequence (getRocket (f r))

fillWithFuel
  :: Rocket a
  -> LaunchSequence 'NoFuel 'HasFuel (Rocket a)
fillWithFuel = LaunchSequence

fillWithOxygene
  :: Rocket a
  -> LaunchSequence 'NoGas 'HasGas (Rocket a)
fillWithOxygene = LaunchSequence

fillWithHeliox
  :: Rocket a
  -> LaunchSequence 'NoGas 'HasGas (Rocket a)
fillWithHeliox = LaunchSequence

ignition
  :: Rocket a
  -> LaunchSequence 'Ready 'Ignited (Rocket a)
ignition = LaunchSequence

boardAstronauts
  :: Rocket a
  -> LaunchSequence 'Ready 'Ready (Rocket a)
boardAstronauts = LaunchSequence

unmannedMission
  :: Rocket a
  -> LaunchSequence 'NoFuel 'Launched (Rocket a)
unmannedMission r =
      return r
  >>= fillWithFuel
  >>= fillWithOxygene
  >>= ignition

mannedMission
  :: Rocket a
  -> LaunchSequence 'NoFuel 'Launched (Rocket a)
mannedMission r =
      return r
  >>= fillWithFuel
  >>= fillWithOxygene
  >>= boardAstronauts
  >>= ignition

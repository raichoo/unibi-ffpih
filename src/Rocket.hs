{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Rocket where

data RocketState
  = NoFuel
  | NoGas
  | Ready
  | Ignited
  | Launched

newtype Rocket (s :: RocketState) a = Rocket
  { unRocket :: a }
  deriving Show

fillWithFuel
  :: Rocket 'NoFuel a
  -> Rocket 'NoGas a
fillWithFuel (Rocket r) = Rocket r

fillWithOxygene
  :: Rocket 'NoGas a
  -> Rocket 'Ready a
fillWithOxygene (Rocket r) = Rocket r

fillWithHeliox
  :: Rocket 'NoGas a
  -> Rocket 'Ready a
fillWithHeliox (Rocket r) = Rocket r

boardAstronauts
  :: Rocket 'Ready a
  -> Rocket 'Ready a
boardAstronauts = id

ignition
  :: Rocket 'Ready a
  -> Rocket 'Launched a
ignition (Rocket r) = Rocket r

mannedMission
  :: Rocket 'NoFuel a
  -> Rocket 'Launched a
mannedMission = ignition . boardAstronauts . fillWithOxygene . fillWithFuel

unmannedMission
  :: Rocket 'NoFuel a
  -> Rocket 'Launched a
unmannedMission = ignition . fillWithOxygene . fillWithFuel

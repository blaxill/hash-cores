{-# LANGUAGE FunctionalDependencies #-}

module Clash.HashCores.Class.Iterable
  where

import           Clash.Prelude

class ( KnownNat rounds
      , KnownNat delay
      , 1 <= rounds
      , 1 <= delay)
      => Iterable x i s o (rounds :: Nat) (delay :: Nat)
         | x -> i, x -> s, x -> o, x -> rounds, x -> delay where

  preIteration :: x -> i -> s
  postIteration :: x -> s -> o

  oneStep :: ( HiddenClockReset domain gated synchronous )
          => x
          -> DSignal domain reference (Index rounds)
             -- ^ Current index
          -> DSignal domain reference s
             -- ^ Some input signal
          -> DSignal domain (reference+delay) s
             -- ^ Some output signal

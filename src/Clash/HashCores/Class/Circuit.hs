{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections          #-}

module Clash.HashCores.Class.Circuit (
  Circuit,
  mkCircuit,
  mkCircuitDataOnly
  )
  where

import           Clash.Prelude

import           Clash.HashCores.Class.Composition

-- | A type that can construct a circuit
class Circuit x inputSemantics i o delay
  | x -> inputSemantics
  , x -> i
  , x -> o
  , x -> delay where
  mkCircuit :: ( HiddenClockResetEnable domain, KnownNat reference)
    => x
    -> DSignal domain reference             (i, Incoming inputSemantics)
    -> ( DSignal domain (reference + delay)  o
       , DSignal domain  reference          (Outgoing inputSemantics) )

-- Convenience when we don't care about data valid/ready signals, this could be
-- that input is ''Always' valid.
mkCircuitDataOnly ::
  ( HiddenClockResetEnable domain, KnownNat reference
  , Circuit x 'Always i o delay)
  => x
  -> DSignal domain reference           i
  -> DSignal domain (reference + delay) o
mkCircuitDataOnly x = fst . mkCircuit x . fmap (,())

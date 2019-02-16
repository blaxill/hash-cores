--    ┌─────┐
-- ─┬▶│  f  ├┬─▶
--  │ └─────┘│
--  └────────┘

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns            #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Composition.Iterated (
  Iterated(..),
  )
  where

import           Clash.Prelude

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable

import           Clash.Signal.Delayed.Bundle as B

-- | A core that iterates in place without explicit storage of state (although
-- implicitly stored in the iterated function), by connecting input to output
-- and tracking iteration indices.
data Iterated = Iterated deriving Show

instance (Iterable iterable _i s _o r d) => Composition Iterated iterable 'ValidReadyFlagged _i s _o r d where
  indexedCompose :: forall domain gated synchronous reference.
      ( HiddenClockReset domain gated synchronous 
      , KnownNat reference)
      => Iterated
      -> iterable
      -- | In signal
      -> DSignal domain reference (s, IsValid)
      -- | Out signal
      -> ( DSignal domain  reference        (IsReady)
         , DSignal domain (reference + r*d)        s  )
  indexedCompose Iterated iterable (B.unbundle -> (input,valid)) =
  -- XXX: Our type safety breaks down in here!
      ( unsafeFromSignal ready
      , unsafeFromSignal state )
    where
      accept = toSignal valid .==. ready

      rounds = snatToNum (SNat @r)

      counters :: Signal domain (Index (3 + d))
      counters = rotatingCountersLE (SNat @d) (rounds+1) SatBound accept

      state :: Signal domain s
      state = toSignal
        ( oneStep iterable
          (unsafeFromSignal $ resize . min (rounds-1) <$> counters)
          (unsafeFromSignal $ mux accept (toSignal input) state)
        )

      ready = (>=) rounds <$> counters

-- | A function to track the values passing through an iterated pipelined component.
-- For a pipeline of n cycles, we store n + 1 counters. The values moving through the
-- pipeline aren't stored, only counters tracking them.
rotatingCounters :: forall domain gated synchronous n a.
                 ( HiddenClockReset domain gated synchronous
                 , SaturatingNum a, Undefined a, KnownNat n)
                 => SNat (n + 1)
                 -> a
                 -> SaturationMode
                 -> Signal domain Bool -- ^ Reset current head at time t
                 -> Signal domain a -- ^ Value of counter reset at time t,t+n,t+2n...
rotatingCounters _slots initial saturationMode reset = mux reset 0 (fmap head counters)
  where
    resetHead True (_:>xs) = 0 :> xs
    resetHead False xs     =      xs

    inc (x:>xs) = xs :< (satAdd saturationMode x 1)

    counters :: Signal domain (Vec (n + 1) a)
    counters = register (repeat initial)
      (inc <$> liftA2 resetHead reset counters)

-- | Convenience wrapper for instantiation with inequality
rotatingCountersLE :: forall domain gated synchronous n a.
                   ( HiddenClockReset domain gated synchronous
                   , SaturatingNum a, Undefined a, KnownNat n
                   , 1 <= n)
                 => SNat n
                 -> a
                 -> SaturationMode
                 -> Signal domain Bool
                 -> Signal domain a
rotatingCountersLE n i s v = leToPlus @1 @n $ rotatingCounters n i s v

-- An iterated construction in which each stage of the hash is processed
-- by the same unit.
--    ┌───────────────┐
-- ─┬▶│  compression  ├┬─▶
--  │ └───────────────┘│
--  └──────────────────┘

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Composition.Iterated (
  Iterated(..),
  )
  where

import           Clash.Prelude

import           Clash.HashCores.Class.Composition

-- | A core that iterates in place with latency of @CompressionDelay hash@ *
-- @Rounds hash@.
data Iterated (slots::Nat) = Iterated deriving Show

instance (KnownNat slots, 1 <= slots) => Composition (Iterated slots) where
  indexedCompose ::
    forall domain gated synchronous t0 t1 x xn1 a .
    ( HiddenClockReset domain gated synchronous
    , KnownNat xn1
    , 1 <= x
    , xn1 ~ (x - 1))
    => Iterated slots
    -> ( forall t0'. DSignal domain t0' (Index x)
        -> DSignal domain t0' a
        -> DSignal domain (t0'+t1) a
       )
    -> DSignal domain t0 a                -- In
    -> DSignal domain t0 Bool             -- Valid
    -> ( DSignal domain (t0+(x*t1)) a  -- Out
       , DSignal domain t0 Bool)          -- Ready
  indexedCompose _iterated f input valid =
  -- XXX: Our safety breaks down in here!
      ( unsafeFromSignal state
      , unsafeFromSignal ready )
    where
      rounds = snatToNum (SNat @x)

      counters :: Signal domain (Index (3 + x))
      counters = rotatingCountersLE (SNat @slots) (rounds+1) SatBound (toSignal valid)

      loadSig = mux (toSignal valid) (toSignal input) state

      state :: Signal domain a
      state = toSignal
        (f
          (unsafeFromSignal $ resize . min (rounds-1) <$> counters)
          (unsafeFromSignal loadSig))

      ready = (>=) rounds <$> counters

rotatingCounters :: forall domain gated synchronous n a.
                 ( HiddenClockReset domain gated synchronous
                 , SaturatingNum a, Undefined a, KnownNat n)
                 => SNat (n + 1)
                 -> a
                 -> SaturationMode
                 -> Signal domain Bool -- ^ Reset current head at time t
                 -> Signal domain a -- ^ Value of counter reset at time t,t+s,t+2s...
rotatingCounters _slots initial saturationMode reset = mux reset 0 (fmap head counters)
  where
    resetHead True (_:>xs) = 0 :> xs
    resetHead False xs     =      xs

    placedCounters = resetHead <$> reset <*> counters

    inc (x:>xs) = xs :< (satAdd saturationMode x 1)

    counters :: Signal domain (Vec (n + 1) a)
    counters = register (repeat initial) (fmap inc placedCounters)

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

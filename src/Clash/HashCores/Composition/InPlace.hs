--    ┌─────┐
-- ─┬▶│  f  ├┬─▶
--  │ └─────┘│
--  └────────┘

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Composition.InPlace (
  InPlace(..),
  )
  where

import           Clash.Prelude

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable

import qualified Clash.Signal.Delayed.Bundle       as B

-- | A core that iterates in place without explicit storage of state (although
-- implicitly stored in the iterated function), by connecting input to output
-- and tracking iteration indices.
data InPlace = InPlace deriving Show

instance (Iterable iterable _i s _o r d) => Composition InPlace iterable 'ValidReadyFlagged _i s _o r d where
  indexedCompose :: forall domain gated synchronous reference.
      ( HiddenClockReset domain gated synchronous
      , KnownNat reference)
      => InPlace
      -> iterable
      -- | In signal
      -> DSignal domain reference (s, IsValid)
      -- | Out signal
      -> ( DSignal domain  reference        (IsReady)
         , DSignal domain (reference + r*d)        s  )
  indexedCompose InPlace iterable (B.unbundle -> (input,valid)) =
  -- XXX: Our type safety breaks down in here!
      ( unsafeFromSignal ready
      , unsafeFromSignal state )
    where
      accept = toSignal valid .==. (ready .==. pure True)

      rounds = snatToNum (SNat @r)

      counter = register 0 counterNext
      counterNext = satAdd SatWrap 1 <$> counter

      ix :: Signal domain (Index (2 + d))
      ix = blockRam
            (replicate (SNat @d) (rounds+1))
            counterNext
            (Just <$> write)

      incr True  _ = 0
      incr False v = v + 1

      write :: Signal domain (Index d, Index (2 + d))
      write = bundle (counter, incr <$> accept <*> ix)

      state :: Signal domain s
      state = toSignal
        ( oneStep iterable
          (unsafeFromSignal $ resize . min (rounds-1) <$> ix)
          (unsafeFromSignal $ mux accept (toSignal input) state)
        )

      ready = (>=) rounds <$> ix

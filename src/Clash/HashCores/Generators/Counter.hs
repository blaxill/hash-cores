{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Generators.Counter (
  counter,
  sha256Counter,
  exampleCounter
  )
  where

import           Clash.Prelude
import           Clash.HashCores.Core

-- For 'exampleCounter'
import           Clash.HashCores.Hash.SHA256
import           Clash.HashCores.Compositions
import           Clash.HashCores.Class.MerkleDamgard

counter :: forall m domain gated synchronous.
        ( HiddenClockReset domain gated synchronous
        , KnownNat m
        )
        => SNat m
        -> Signal domain (BitVector (m*8))
counter _ = count
  where
    count :: Signal domain (BitVector (m*8))
    count = register 0 (count + 1)

sha256Counter :: forall m m0 domain gated synchronous.
        ( HiddenClockReset domain gated synchronous
        , KnownNat m
        , KnownNat m0
        , (m*8) <= 447
        , ((m*8)+m0) ~ 448
        )
        => SNat m
        -> Signal domain (BitVector (m*8))
        -> Signal domain (BitVector 512)
sha256Counter bytes count = flip preprocessBytes (natVal bytes) <$> count

-- | Ex.
-- >>> mapM_ printX  $ sampleN 200 $ bundle (exampleCounter d4)
exampleCounter ::
     ( HiddenClockReset domain gated synchronous
     , KnownNat m
     , KnownNat m0
     , (m*8) <= 447
     , ((m*8)+m0) ~ 448
     )
     => SNat m ->
     ( Signal domain (BitVector (m*8))
     , Signal domain (BitVector 256)
     , Signal domain Bool -- Valid
     )
exampleCounter bytes = (delayedVal,hash,valid)
  where
    hash = toSignal $ singleBlockPipe (Pipelined :. SHA256 @1 @1) preprocessed
    preprocessed = fromSignal $ sha256Counter bytes val
    val = counter bytes
    delaySNat = SNat
      @(CompressionDelay (SHA256 1 1)
      * CompressionRounds (SHA256 1 1)
      )
    delayedVal = toSignal $ delayN delaySNat (fromSignal val)
    valid = (<=) (fromInteger $ natVal delaySNat) <$> val
